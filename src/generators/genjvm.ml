(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Globals
open Ast
open Common
open Type
open Path
open JvmGlobals
open MethodAccessFlags
open FieldAccessFlags
open JvmData
open JvmAttribute
open JvmSignature
open JvmMethod
open JvmBuilder
open Genshared

(* Note: This module is the bridge between Haxe structures and JVM structures. No module in generators/jvm should reference any
   Haxe-specific type. *)

(* hacks *)

let is_really_int t =
	not (is_nullable t) && ExtType.is_int (follow t)

let get_construction_mode c cf =
	if Meta.has Meta.HxGen cf.cf_meta then ConstructInitPlusNew
	else ConstructInit

(* Haxe *)

type generation_context = {
	com : Common.context;
	jar : Zip.out_file;
	t_runtime_exception : Type.t;
	entry_point : (tclass * texpr) option;
	t_exception : Type.t;
	t_throwable : Type.t;
	mutable anon_identification : jsignature tanon_identification;
	mutable preprocessor : jsignature preprocessor;
	default_export_config : export_config;
	typed_functions : JvmFunctions.typed_functions;
	closure_paths : (path * string * jsignature,path) Hashtbl.t;
	enum_paths : (path,unit) Hashtbl.t;
	detail_times : bool;
	mutable timer : Timer.timer;
	mutable typedef_interfaces : jsignature typedef_interfaces;
	mutable current_field_info : field_generation_info option;
	jar_compression_level : int;
	dynamic_level : int;
}

type ret =
	| RValue of jsignature option
	| RVoid
	| RReturn

type access_kind =
	| AKPost
	| AKPre
	| AKNone

type compare_kind =
	| CmpNormal of jcmp * jsignature
	| CmpSpecial of (jbranchoffset ref -> unit)

type block_exit =
	| ExitExecute of (unit -> unit)
	| ExitLoop

let need_val = function
	| RValue _ -> true
	| _ -> false

let run_timed gctx detail name f =
	if detail && not gctx.detail_times then
		f()
	else begin
		let sub = gctx.timer#nest name in
		let old = gctx.timer in
		gctx.timer <- sub;
		sub#run_finally f (fun () -> gctx.timer <- old)
	end

open NativeSignatures

let rec jsignature_of_type gctx stack t =
	if List.exists (fast_eq t) stack then object_sig else
	let jsignature_of_type = jsignature_of_type gctx (t :: stack) in
	let jtype_argument_of_type t = jtype_argument_of_type gctx stack t in
	match t with
	| TAbstract(a,tl) ->
		begin match a.a_path with
			| [],"Bool" -> TBool
			| ["java"],"Int8" -> TByte
			| ["java"],"Int16" -> TShort
			| [],"Int" -> TInt
			| ["haxe"],"Int32" -> TInt
			| ["haxe"],"Int64" -> TLong
			| ["java"],"Int64" -> TLong
			| ["java"],"Char16" -> TChar
			| [],"Single" -> TFloat
			| [],"Float" -> TDouble
			| [],"Void" -> void_sig
			| [],"Null" ->
				begin match tl with
				| [t] -> get_boxed_type (jsignature_of_type t)
				| _ -> die "" __LOC__
				end
			| ["haxe";"ds"],"Vector" ->
				begin match tl with
				| [t] -> TArray(jsignature_of_type t,None)
				| _ -> die "" __LOC__
				end
			| ["haxe"],"Rest" ->
				begin match tl with
				| [t] -> TArray(get_boxed_type (jsignature_of_type t),None)
				| _ -> die "" __LOC__
				end
			(* | ["haxe"],"Rest" -> TArray(object_sig,None) *)
			| [],"Dynamic" ->
				object_sig
			| [],("Class" | "Enum") ->
				begin match tl with
				| [t] -> TObject(java_class_path,[TType(WNone,jsignature_of_type t)])
				| _ -> java_class_sig
				end
			| [],"EnumValue" ->
				java_enum_sig object_sig
			| _ ->
				if Meta.has Meta.CoreType a.a_meta then
					TObject(a.a_path,List.map jtype_argument_of_type tl)
				else
					jsignature_of_type (Abstract.get_underlying_type a tl)
		end
	| TDynamic _ -> object_sig
	| TMono r ->
		begin match r.tm_type with
		| Some t -> jsignature_of_type t
		| None -> object_sig
		end
	| TInst({cl_path = (["haxe";"root"],"String")},[]) -> string_sig
	| TInst({cl_path = (["haxe";"root"],"Array")},[t]) ->
		let t = get_boxed_type (jsignature_of_type t) in
		TObject((["haxe";"root"],"Array"),[TType(WNone,t)])
	| TInst({cl_path = (["java"],"NativeArray")},[t]) ->
		TArray(jsignature_of_type t,None)
	| TInst({cl_kind = KTypeParameter [t]},_) -> jsignature_of_type t
	| TInst({cl_kind = KTypeParameter _; cl_path = (_,name)},_) -> TTypeParameter name
	| TInst({cl_path = ["_Class"],"Class_Impl_"},_) -> java_class_sig
	| TInst({cl_path = ["_Enum"],"Enum_Impl_"},_) -> java_class_sig
	| TInst(c,tl) -> TObject(c.cl_path,List.map jtype_argument_of_type tl)
	| TEnum(en,tl) ->
		Hashtbl.replace gctx.enum_paths en.e_path ();
		TObject(en.e_path,List.map jtype_argument_of_type tl)
	| TFun(tl,tr) -> method_sig (List.map (fun (_,o,t) ->
		let jsig = jsignature_of_type t in
		let jsig = if o then get_boxed_type jsig else jsig in
		jsig
	) tl) (return_of_type gctx stack tr)
	| TAnon an -> object_sig
	| TType({ t_path = ["haxe"],"Rest$NativeRest" },[t]) ->
		TArray(get_boxed_type (jsignature_of_type t),None)
	| TType(td,tl) ->
		begin match gctx.typedef_interfaces#get_interface_class td.t_path with
		| Some c -> TObject(c.cl_path,[])
		| None -> jsignature_of_type (apply_typedef td tl)
		end
	| TLazy f -> jsignature_of_type (lazy_type f)

and jtype_argument_of_type gctx stack t =
	let jsig = jsignature_of_type gctx stack t in
	let jsig = get_boxed_type jsig in
	TType(WNone,jsig)

and return_of_type gctx stack t =
	if ExtType.is_void (follow t) then None else Some (jsignature_of_type gctx stack t)

let jsignature_of_type gctx t =
	jsignature_of_type gctx [] t

let return_of_type gctx t =
	return_of_type gctx [] t

let convert_fields gctx pfm =
	match pfm.pfm_converted with
	| Some l ->
		l
	| None ->
		let l = PMap.foldi (fun s cf acc -> (s,cf) :: acc) pfm.pfm_fields [] in
		let l = List.sort (fun (s1,_) (s2,_) -> compare s1 s2) l in
		let l = List.map (fun (s,cf) -> s,jsignature_of_type gctx cf.cf_type) l in
		pfm.pfm_converted <- Some l;
		l

module AnnotationHandler = struct
	let convert_annotations meta =
		let parse_path e =
			let sl = try string_list_of_expr_path_raise e with Exit -> Error.typing_error "Field expression expected" (pos e) in
			let path = match sl with
				| s :: sl -> List.rev sl,s
				| _ -> Error.typing_error "Field expression expected" (pos e)
			in
			path
		in
		let rec parse_value e = match fst e with
			| EConst (Int s) -> AInt (Int32.of_string s)
			| EConst (Float s) -> ADouble (float_of_string s)
			| EConst (String(s,_)) -> AString s
			| EConst (Ident "true") -> ABool true
			| EConst (Ident "false") -> ABool false
			| EArrayDecl el -> AArray (List.map parse_value el)
			| EField(e1,s) ->
				let path = parse_path e1 in
				AEnum(object_path_sig path,s)
			| _ -> Error.typing_error "Expected value expression" (pos e)
		in
		let parse_value_pair e = match fst e with
			| EBinop(OpAssign,(EConst(Ident s),_),e1) ->
				s,parse_value e1
			| _ ->
				Error.typing_error "Assignment expression expected" (pos e)
		in
		let parse_expr e = match fst e with
			| ECall(e1,el) ->
				let path = parse_path e1 in
				let _,name = ExtString.String.replace (snd path) "." "$" in
				let path = (fst path,name) in
				let values = List.map parse_value_pair el in
				path,values
			| _ ->
				Error.typing_error "Call expression expected" (pos e)
		in
		ExtList.List.filter_map (fun (m,el,_) -> match m,el with
			| Meta.Meta,[e] ->
				let path,annotation = parse_expr e in
				let path = match path with
					| [],name -> ["haxe";"root"],name
					| _ -> path
				in
				Some(path,annotation)
			| _ ->
				None
		) meta

	let generate_annotations builder meta =
		List.iter (fun (path,annotation) ->
			builder#add_annotation path annotation
		) (convert_annotations meta)
end

let enum_ctor_sig =
	let ta = TArray(object_sig,None) in
	method_sig [TInt;ta] None

let convert_cmp_op = function
	| OpEq -> CmpEq
	| OpNotEq -> CmpNe
	| OpLt -> CmpLt
	| OpLte -> CmpLe
	| OpGt -> CmpGt
	| OpGte -> CmpGe
	| _ -> die "" __LOC__

let flip_cmp_op = function
	| CmpEq -> CmpNe
	| CmpNe -> CmpEq
	| CmpLt -> CmpGe
	| CmpLe -> CmpGt
	| CmpGt -> CmpLe
	| CmpGe -> CmpLt

let resolve_class com path =
	let rec loop types = match types with
		| (TClassDecl c) :: types when c.cl_path = path ->
			c
		| _ :: types ->
			loop types
		| [] ->
			jerror ("No such type: " ^ s_type_path path)
	in
	loop com.types

let write_class gctx path jc =
	let dir = match path with
		| ([],s) -> s
		| (sl,s) -> String.concat "/" sl ^ "/" ^ s
	in
	let path = dir ^ ".class" in
	let t = Timer.timer ["jvm";"write"] in
	let ch = IO.output_bytes() in
	JvmWriter.write_jvm_class ch jc;
	Zip.add_entry ~level:gctx.jar_compression_level (Bytes.unsafe_to_string (IO.close_out ch)) gctx.jar path;
	t()

let is_const_int_pattern (el,_) =
	List.for_all (fun e -> match e.eexpr with
		| TConst (TInt _) -> true
		| _ -> false
	) el

let is_const_string_pattern (el,_) =
	List.for_all (fun e -> match e.eexpr with
		| TConst (TString _) -> true
		| _ -> false
	) el

let is_interface_var_access c cf =
	(has_class_flag c CInterface) && match cf.cf_kind with
		| Var _ | Method MethDynamic -> true
		| _ -> false

let follow = Abstract.follow_with_abstracts

class haxe_exception gctx (t : Type.t) =
	let is_haxe_exception = Exceptions.is_haxe_exception t
	and native_type = jsignature_of_type gctx t in
object(self)
	val native_path = (match native_type with TObject(path,_) -> path | _ -> die "" __LOC__)

	method is_assignable_to (exc2 : haxe_exception) =
		match self#is_haxe_exception,exc2#is_haxe_exception with
		| true, true | false, false ->
			does_unify t exc2#get_type
		(* `haxe.Exception` is assignable to java.lang.RuntimeException/Exception/Throwable *)
		| false,true ->
			List.mem exc2#get_native_type [throwable_sig; exception_sig; runtime_exception_sig]
		| _ ->
			false

	method is_haxe_exception = is_haxe_exception

	method get_native_type = native_type
	method get_native_path = native_path
	method get_type = t
end

let generate_equals_function (jc : JvmClass.builder) jsig_arg =
	let jm_equals = jc#spawn_method "equals" (method_sig [jsig_arg] (Some TBool)) [MPublic] in
	let code = jm_equals#get_code in
	let _,load,_ = jm_equals#add_local "other" jsig_arg VarArgument in
	jm_equals#finalize_arguments;
	load();
	code#instanceof jc#get_this_path;
	jm_equals#if_then
		(code#if_ CmpNe)
		(fun () ->
			code#bconst false;
			jm_equals#return;
		);
	load();
	let _,load,save = jm_equals#add_local "other" jc#get_jsig VarWillInit in
	jm_equals#cast jc#get_jsig;
	save();
	jm_equals,load

let create_field_closure gctx jc path_this jm name jsig =
	let jsig_this = object_path_sig path_this in
	let context = ["this",jsig_this] in
	let wf = new JvmFunctions.typed_function gctx.typed_functions (FuncMember(path_this,name)) jc jm context in
	let jc_closure = wf#get_class in
	ignore(wf#generate_constructor true);
	let args,ret = match jsig with
		| TMethod(args,ret) ->
			List.mapi (fun i jsig -> (Printf.sprintf "arg%i" i,jsig)) args,ret
		| _ ->
			die "" __LOC__
	in
	let jm_invoke = wf#generate_invoke args ret in
	let vars = List.map (fun (name,jsig) ->
		jm_invoke#add_local name jsig VarArgument
	) args in
	jm_invoke#finalize_arguments;
	jm_invoke#load_this;
	jm_invoke#getfield jc_closure#get_this_path "this" jsig_this;
	List.iter (fun (_,load,_) ->
		load();
	) vars;
	jm_invoke#invokevirtual path_this name (method_sig (List.map snd args) ret);
	jm_invoke#return;
	(* equals *)
	begin
		let jm_equals,load = generate_equals_function jc_closure object_sig in
		let code = jm_equals#get_code in
		jm_equals#load_this;
		jm_equals#getfield jc_closure#get_this_path "this" jsig_this;
		load();
		jm_equals#getfield jc_closure#get_this_path "this" jsig_this;
		jm_equals#if_then
			(code#if_acmp_eq jc_closure#get_jsig jc_closure#get_jsig)
			(fun () ->
				code#bconst false;
				jm_equals#return;
			);
		code#bconst true;
		jm_equals#return;
	end;
	write_class gctx jc_closure#get_this_path (jc_closure#export_class gctx.default_export_config);
	jc_closure#get_this_path

let create_field_closure gctx jc path_this jm name jsig f =
	let jsig_this = object_path_sig path_this in
	let closure_path = try
		Hashtbl.find gctx.closure_paths (path_this,name,jsig)
	with Not_found ->
		let closure_path = create_field_closure gctx jc path_this jm name jsig in
		Hashtbl.add gctx.closure_paths (path_this,name,jsig) closure_path;
		closure_path
	in
	jm#construct ConstructInit closure_path (fun () ->
		f();
		[jsig_this]
	)

let rvalue_any = RValue None
let rvalue_sig jsig = RValue (Some jsig)
let rvalue_type gctx t = RValue (Some (jsignature_of_type gctx t))

class texpr_to_jvm gctx (jc : JvmClass.builder) (jm : JvmMethod.builder) (return_type : jsignature option) = object(self)
	val com = gctx.com
	val code = jm#get_code
	val pool : JvmConstantPool.constant_pool = jc#get_pool

	val mutable local_lookup = Hashtbl.create 0;
	val mutable last_line = 0

	val mutable break = None
	val mutable continue = None
	val mutable caught_exceptions = []
	val mutable block_exits = []
	val mutable env = None

	method vtype t =
		jsignature_of_type gctx t

	method mknull t = com.basic.tnull (follow t)

	(* locals *)

	method add_named_local (name : string) (jsig : jsignature) =
		jm#add_local name jsig VarArgument

	method add_local v init_state : (int * (unit -> unit) * (unit -> unit)) =
		let t = self#vtype v.v_type in
		let slot,load,store = jm#add_local v.v_name t init_state in
		Hashtbl.add local_lookup v.v_id (slot,load,store);
		slot,load,store

	method get_local_by_id (vid,vname) =
		if vid = 0 && env = None then
			(0,(fun () -> jm#load_this),(fun () -> die "" __LOC__))
		else try
			Hashtbl.find local_lookup vid
		with Not_found -> try
			begin match env with
			| Some env ->
				let name,jsig = List.assoc vid env in
				(-1,
					(fun () ->
						jm#load_this;
						jm#getfield jc#get_this_path name jsig
					),
					(fun () ->
						jm#load_this;
						jm#putfield jc#get_this_path name jsig
					)
				)
			| None ->
				raise Not_found
			end
		with Not_found ->
			failwith ("Unbound local: " ^ vname)

	method get_local v =
		self#get_local_by_id (v.v_id,v.v_name)

	method set_env (env' : (int * (string * jsignature)) list) =
		env <- Some env'

	(* casting *)

	method expect_reference_type = jm#expect_reference_type

	method cast t =
		let vt = self#vtype t in
		jm#cast vt

	method cast_expect ret t = match ret with
		| RValue (Some jsig) -> jm#cast jsig
		| _ -> self#cast t

	method make_static_closure_field (name : string) (jc_closure : JvmClass.builder) =
		let jm_init = jc_closure#get_static_init_method in
		let jf_closure = jc_closure#spawn_field name jc_closure#get_jsig [FdStatic;FdPublic;FdFinal] in
		jm_init#construct ConstructInit jc_closure#get_this_path (fun () -> []);
		jm_init#putstatic jc_closure#get_this_path jf_closure#get_name jf_closure#get_jsig;

	method tfunction e tf =
		let outside,accesses_this = Texpr.collect_captured_vars e in
		let env = List.map (fun v ->
			v.v_id,(v.v_name,self#vtype v.v_type)
		) outside in
		let env = if accesses_this then ((0,("this",jc#get_jsig)) :: env) else env in
		let context = List.map snd env in
		let wf = new JvmFunctions.typed_function gctx.typed_functions FuncLocal jc jm context in
		let jc_closure = wf#get_class in
		ignore(wf#generate_constructor (env <> []));
		let args,ret =
			let args = List.map (fun (v,eo) ->
				(* TODO: Can we do this differently? *)
				if eo <> None then v.v_type <- self#mknull v.v_type;
				v.v_name,self#vtype v.v_type
			) tf.tf_args in
			args,(return_of_type gctx tf.tf_type)
		in
		let jm_invoke = wf#generate_invoke args ret in
		let handler = new texpr_to_jvm gctx jc_closure jm_invoke ret in
		handler#set_env env;
		let args = List.map (fun (v,eo) ->
			handler#add_local v VarArgument,v,eo
		) tf.tf_args in
		jm_invoke#finalize_arguments;
		List.iter (fun ((_,load,save),v,eo) -> match eo with
			| Some e when (match e.eexpr with TConst TNull -> false | _ -> true) ->
				load();
				let jsig = self#vtype v.v_type in
				jm_invoke#if_then
					(jm_invoke#get_code#if_nonnull jsig)
					(fun () ->
						handler#texpr (rvalue_sig jsig) e;
						jm_invoke#cast jsig;
						save();
					)
			| _ ->
				()
		) args;
		handler#texpr RReturn tf.tf_expr;
		begin match env with
		| [] ->
			let name = snd jc_closure#get_this_path in
			self#make_static_closure_field name jc_closure;
			jm#getstatic jc_closure#get_this_path name (object_path_sig jc_closure#get_this_path);
		| _ ->
			jm#construct ConstructInit jc_closure#get_this_path (fun () ->
				(List.map (fun (id,(name,jsig)) ->
					let _,load,_ = self#get_local_by_id (id,name) in
					load();
					jsig
				) env);
			);
		end;
		write_class gctx jc_closure#get_this_path (jc_closure#export_class gctx.default_export_config);

	(* access *)

	method read_native_array vta vte =
		NativeArray.read code vta vte

	method write_native_array vta vte =
		NativeArray.write code vta vte

	method read_anon_field cast t cf =
		let default () =
			jm#string cf.cf_name;
			jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
			cast();
		in
		match gctx.anon_identification#identify true t with
		| Some pfm ->
			let cf = PMap.find cf.cf_name pfm.pfm_fields in
			let path = pfm.pfm_path in
			code#dup;
			code#instanceof path;
			jm#if_then_else
				(code#if_ CmpEq)
				(fun () ->
					jm#cast (object_path_sig path);
					jm#getfield path cf.cf_name (self#vtype cf.cf_type);
					cast();
				)
				(fun () -> default());
		| None ->
			default();

	method read_static_closure (path : path) (name : string) (args : (string * jsignature) list) (ret : jsignature option) =
		let jsig = method_sig (List.map snd args) ret in
		let closure_path = try
			Hashtbl.find gctx.closure_paths (path,name,jsig)
		with Not_found ->
			let wf = new JvmFunctions.typed_function gctx.typed_functions (FuncStatic(path,name)) jc jm [] in
			let jc_closure = wf#get_class in
			ignore(wf#generate_constructor false);
			let jm_invoke = wf#generate_invoke args ret in
			let vars = List.map (fun (name,jsig) ->
				jm_invoke#add_local name jsig VarArgument
			) args in
			jm_invoke#finalize_arguments;
			List.iter (fun (_,load,_) ->
				load();
			) vars;
			jm_invoke#invokestatic path name (method_sig (List.map snd args) ret);
			jm_invoke#return;
			Hashtbl.add gctx.closure_paths (path,name,jsig) jc_closure#get_this_path;
			(* Static init *)
			self#make_static_closure_field name jc_closure;
			write_class gctx jc_closure#get_this_path (jc_closure#export_class gctx.default_export_config);
			jc_closure#get_this_path;
		in
		jm#getstatic closure_path name (object_path_sig closure_path);

	method read cast e1 fa =
		let read_static_closure path cf =
			let args,ret = match follow cf.cf_type with
				| TFun(tl,tr) -> List.map (fun (n,_,t) -> n,self#vtype t) tl,(return_of_type gctx tr)
				| _ -> die "" __LOC__
			in
			self#read_static_closure path cf.cf_name args ret
		in
		let dynamic_read s =
			self#texpr rvalue_any e1;
			jm#string s;
			jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
			cast();
		in
		match fa with
		| FStatic({cl_path = (["java";"lang"],"Math")},({cf_name = "NaN" | "POSITIVE_INFINITY" | "NEGATIVE_INFINITY"} as cf)) ->
			jm#getstatic double_path cf.cf_name TDouble
		| FStatic({cl_path = (["java";"lang"],"Math")},({cf_name = "isNaN" | "isFinite"} as cf)) ->
			read_static_closure double_path cf;
		| FStatic({cl_path = (["java";"lang"],"String")},({cf_name = "fromCharCode"} as cf)) ->
			read_static_closure (["haxe";"jvm"],"StringExt") cf
		| FStatic(c,({cf_kind = Method (MethNormal | MethInline)} as cf)) ->
			read_static_closure c.cl_path cf
		| FStatic(c,cf) ->
			jm#getstatic c.cl_path cf.cf_name (self#vtype cf.cf_type);
			cast();
		| FInstance({cl_path = (["java";"lang"],"String")},_,{cf_name = "length"}) ->
			self#texpr rvalue_any e1;
			jm#invokevirtual string_path "length" (method_sig [] (Some TInt))
		| FInstance({cl_path = (["java"],"NativeArray")},_,{cf_name = "length"}) ->
			self#texpr rvalue_any e1;
			let vtobj = self#vtype e1.etype in
			code#arraylength vtobj;
		| FInstance(c,tl,cf) | FClosure(Some(c,tl),({cf_kind = Method MethDynamic} as cf)) when not (is_interface_var_access c cf) ->
			self#texpr rvalue_any e1;
			jm#getfield c.cl_path cf.cf_name (self#vtype cf.cf_type);
			cast();
		| FEnum(en,ef) when not (match follow ef.ef_type with TFun _ -> true | _ -> false) ->
			let jsig = self#vtype ef.ef_type in
			let offset = pool#add_field en.e_path ef.ef_name jsig FKField in
			code#getstatic offset jsig;
			cast();
		| FAnon cf ->
			self#texpr rvalue_any e1;
			self#read_anon_field cast e1.etype cf;
		| FDynamic s | FInstance(_,_,{cf_name = s}) | FEnum(_,{ef_name = s}) | FClosure(None,{cf_name = s}) ->
			dynamic_read s
		| FClosure((Some(c,_)),cf) ->
			if has_class_flag c CInterface then
				dynamic_read cf.cf_name
			else
				create_field_closure gctx jc c.cl_path jm cf.cf_name (self#vtype cf.cf_type) (fun () ->
					self#texpr rvalue_any e1;
				)

	method read_write ret ak e (f : unit -> unit) =
		let apply dup =
			if need_val ret && ak = AKPost then dup();
			f();
			if need_val ret && ak <> AKPost then dup();
		in
		let default s t =
			if ak <> AKNone then code#dup;
			jm#string s;
			if ak <> AKNone then begin
				code#dup_x1;
				jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
				self#cast_expect ret t;
			end;
			apply (fun () -> code#dup_x2);
			self#cast (self#mknull t);
			jm#invokestatic haxe_jvm_path "writeField" (method_sig [object_sig;string_sig;object_sig] None)
		in
		match (Texpr.skip e).eexpr with
		| TLocal v ->
			let _,load,store = self#get_local v in
			if ak <> AKNone then load();
			apply (fun () -> code#dup);
			self#cast v.v_type;
			store();
		| TField(_,FStatic(c,cf)) ->
			let jsig_cf = self#vtype cf.cf_type in
			if ak <> AKNone then jm#getstatic c.cl_path cf.cf_name jsig_cf;
			apply (fun () -> code#dup);
			jm#cast jsig_cf;
			jm#putstatic c.cl_path cf.cf_name jsig_cf;
		| TField(e1,FInstance(c,tl,cf)) when not (is_interface_var_access c cf) ->
			self#texpr rvalue_any e1;
			let jsig_cf = self#vtype cf.cf_type in
			if ak <> AKNone then begin
				code#dup;
				jm#getfield c.cl_path cf.cf_name jsig_cf
			end;
			apply (fun () -> code#dup_x1);
			self#cast cf.cf_type;
			jm#putfield c.cl_path cf.cf_name jsig_cf
		| TField(e1,FAnon cf) ->
			self#texpr rvalue_any e1;
			begin match gctx.anon_identification#identify true e1.etype with
			| Some pfm ->
				let cf = PMap.find cf.cf_name pfm.pfm_fields in
				let path = pfm.pfm_path in
				code#dup;
				code#instanceof path;
				let jsig_cf = self#vtype cf.cf_type in
				jm#if_then_else
					(code#if_ CmpEq)
					(fun () ->
						jm#cast (object_path_sig path);
						if ak <> AKNone then begin
							code#dup;
							jm#getfield path cf.cf_name jsig_cf;
						end;
						apply (fun () -> code#dup_x1);
						jm#cast jsig_cf;
						jm#putfield path cf.cf_name jsig_cf;
					)
					(fun () ->
						default cf.cf_name cf.cf_type;
						if need_val ret then jm#cast jsig_cf;
					);
			| None ->
				default cf.cf_name cf.cf_type;
			end
		| TField(e1,(FDynamic s | FInstance(_,_,{cf_name = s}))) ->
			self#texpr rvalue_any e1;
			default s e.etype;
		| TArray(e1,e2) ->
			begin match follow e1.etype with
				| TInst({cl_path = (["haxe";"root"],"Array")} as c,[t]) ->
					self#texpr rvalue_any e1;
					if ak <> AKNone then code#dup;
					self#texpr rvalue_any e2;
					jm#cast TInt;
					if ak <> AKNone then begin
						code#dup_x1;
						jm#invokevirtual c.cl_path "__get" (method_sig [TInt] (Some object_sig));
						self#cast_expect ret e.etype;
					end;
					apply (fun () -> code#dup_x2;);
					jm#expect_reference_type;
					jm#invokevirtual c.cl_path "__set" (method_sig [TInt;object_sig] None);
				| TInst({cl_path = (["java"],"NativeArray")},[t]) ->
					let vte = self#vtype t in
					let vta = self#vtype e1.etype in
					self#texpr rvalue_any e1;
					if ak <> AKNone then code#dup;
					self#texpr rvalue_any e2;
					if ak <> AKNone then begin
						code#dup_x1;
						self#read_native_array vta vte
					end;
					apply (fun () -> code#dup_x2);
					self#cast t;
					self#write_native_array vta vte
				| _ ->
					self#texpr rvalue_any e1;
					if ak <> AKNone then code#dup;
					self#texpr rvalue_any e2;
					jm#cast TInt;
					if ak <> AKNone then begin
						code#dup_x1;
						jm#invokestatic haxe_jvm_path "arrayRead" (method_sig [object_sig;TInt] (Some object_sig));
					end;
					apply (fun () -> code#dup_x2;);
					self#cast e.etype;
					self#expect_reference_type;
					jm#invokestatic haxe_jvm_path "arrayWrite" (method_sig [object_sig;TInt;object_sig] None);
				end
		| _ ->
			print_endline (s_expr_ast false "" (s_type (print_context())) e);
			die "" __LOC__

	(* branching *)

	method apply_cmp = function
		| CmpNormal(op,_) -> code#if_ op
		| CmpSpecial f -> f

	method condition (flip : bool) (e : texpr) (label_then : label) (label_else : label) =
		let stack = jm#get_code#get_stack in
		let (_,before) = stack#save in
		let bool_and flip e1 e2 =
			let label_then2 = jm#spawn_label "then2" in
			self#condition flip e1 label_then2 label_else;
			label_then2#here;
			self#condition flip e2 label_then label_else;
		in
		let bool_or flip e1 e2 =
			let label_else2 = jm#spawn_label "else2" in
			self#condition (not flip) e1 label_else2 label_then;
			label_else2#here;
			self#condition flip e2 label_then label_else;
		in
		let involves_float_compare e =
			let rec loop e = match e.eexpr with
				| TBinop((OpEq | OpNotEq | OpLt | OpGt | OpLte | OpGte),e1,e2) ->
					if ExtType.is_float (follow e1.etype) || ExtType.is_float (follow e2.etype) then raise Exit;
					loop e1;
					loop e2;
				| _ ->
					Type.iter loop e
			in
			try
				loop e;
				false
			with Exit ->
				true
		in
		begin match (Texpr.skip e).eexpr with
		| TBinop((OpEq | OpNotEq | OpLt | OpGt | OpLte | OpGte) as op,e1,e2) ->
			let op = convert_cmp_op op in
			let op = if flip then flip_cmp_op op else op in
			label_else#apply (self#apply_cmp (self#binop_compare op e1 e2))
		| TBinop(OpBoolAnd,e1,e2) when not flip ->
			bool_and false e1 e2
		| TBinop(OpBoolAnd,e1,e2) when not (involves_float_compare e1) ->
			bool_or true e1 e2
		| TBinop(OpBoolOr,e1,e2) when flip ->
			bool_and true e1 e2
		| TBinop(OpBoolOr,e1,e2) when not (involves_float_compare e1) ->
			bool_or false e1 e2
		| TUnop(Not,_,e1) when not (involves_float_compare e1) ->
			self#condition (not flip) e1 label_then label_else
		| _ ->
			self#texpr (rvalue_sig TBool) e;
		end;
		let (_,after) = stack#save in
		if after > before then begin
			jm#cast TBool;
			label_else#if_ (if flip then CmpNe else CmpEq)
		end

	method switch ret e1 cases def =
		let need_val = match ret with
			| RValue _ -> true
			| RReturn -> return_type <> None
			| _ -> false
		in
		if cases = [] then
			self#texpr ret e1
		else if List.for_all is_const_int_pattern cases then begin
			let cases = List.map (fun (el,e) ->
				let il = List.map (fun e -> match e.eexpr with
					| TConst (TInt i32) -> i32
					| _ -> die "" __LOC__
				) el in
				(il,(fun () -> self#texpr ret e))
			) cases in
			let def = match def with
				| None -> None
				| Some e -> Some (fun () -> self#texpr ret e)
			in
			self#texpr rvalue_any e1;
			jm#cast TInt;
			jm#int_switch need_val cases def
		end else if List.for_all is_const_string_pattern cases then begin
			let cases = List.map (fun (el,e) ->
				let sl = List.map (fun e -> match e.eexpr with
					| TConst (TString s) -> s
					| _ -> die "" __LOC__
				) el in
				(sl,(fun () -> self#texpr ret e))
			) cases in
			let def = match def with
				| None -> None
				| Some e -> Some (fun () -> self#texpr ret e)
			in
			self#texpr rvalue_any e1;
			jm#cast string_sig;
			let _,load,save = jm#add_local "_hx_tmp" string_sig VarWillInit in
			save();
			jm#string_switch need_val load cases def;
		end else begin
			(* TODO: rewriting this is stupid *)
			let pop_scope = jm#push_scope in
			self#texpr rvalue_any e1;
			let v = alloc_var VGenerated "tmp" e1.etype null_pos in
			let _,_,store = self#add_local v VarWillInit in
			self#cast v.v_type;
			store();
			let ev = mk (TLocal v) v.v_type null_pos in
			let el = List.rev_map (fun (el,e) ->
				let f e' = mk (TBinop(OpEq,ev,e')) com.basic.tbool e'.epos in
				let e_cond = match el with
					| [] -> die "" __LOC__
					| [e] -> f e
					| e :: el ->
						List.fold_left (fun eacc e ->
							mk (TBinop(OpBoolOr,eacc,f e)) com.basic.tbool e.epos
						) (f e) el
				in
				(e_cond,e)
			) cases in
			(* If we rewrite an exhaustive switch that has no default value, treat the last case as the default case to satisfy control flow. *)
			let cases,def = if need_val && def = None then (match List.rev cases with (_,e) :: cases -> List.rev cases,Some e | _ -> die "" __LOC__) else cases,def in
			let e = List.fold_left (fun e_else (e_cond,e_then) -> Some (mk (TIf(e_cond,e_then,e_else)) e_then.etype e_then.epos)) def el in
			self#texpr ret (Option.get e);
			pop_scope()
		end

	(* binops *)

	method binop_exprs cast_type f1 f2 =
		f1 (rvalue_sig cast_type);
		jm#cast cast_type;
		f2 (rvalue_sig cast_type);
		jm#cast cast_type;

	method get_binop_type_sig jsig1 jsig2 =
		match get_unboxed_type jsig1, get_unboxed_type jsig2 with
		| TObject((["java";"lang"],"String"),_),_
		| _,TObject((["java";"lang"],"String"),_) ->
			string_sig
		| TLong,_ | _,TLong -> TLong
		| TDouble,_ | _,TDouble -> TDouble
		| TFloat,_ | _,TFloat -> TFloat
		| TInt,_ | _,TInt -> TInt
		| TShort,_ | _,TShort -> TShort
		| TChar,_ | _,TChar -> TChar
		| TByte,_ | _,TByte -> TByte
		| TBool,_ | _,TBool -> TBool
		| jsig1,jsig2 ->
			if jsig1 = string_sig || jsig2 = string_sig then
				string_sig
			else
				object_sig

	method get_binop_type t1 t2 = self#get_binop_type_sig (jsignature_of_type gctx t1) (jsignature_of_type gctx t2)

	method do_compare op =
		match code#get_stack#get_stack_items 2 with
		| [TInt | TByte | TChar | TBool;TInt | TByte | TChar | TBool] ->
			let op = flip_cmp_op op in
			CmpSpecial (code#if_icmp op)
		| [TObject((["java";"lang"],"String"),[]);TObject((["java";"lang"],"String"),[])] ->
			jm#invokestatic haxe_jvm_path "stringCompare" (method_sig [string_sig;string_sig] (Some TInt));
			let op = flip_cmp_op op in
			CmpNormal(op,TBool)
		| [TObject((["java";"lang"],"Object"),[]) | TTypeParameter _;_]
		| [_;TObject((["java";"lang"],"Object"),[]) | TTypeParameter _] ->
			jm#invokestatic haxe_jvm_path "compare" (method_sig [object_sig;object_sig] (Some TInt));
			let op = flip_cmp_op op in
			CmpNormal(op,TBool)
		| [(TObject _ | TArray _ | TMethod _) as t1;(TObject _ | TArray _ | TMethod _) as t2] ->
			CmpSpecial ((if op = CmpEq then code#if_acmp_ne else code#if_acmp_eq) t1 t2)
		| [TDouble;TDouble] ->
			let op = flip_cmp_op op in
			begin match op with
			| CmpGe | CmpGt -> code#dcmpg;
			| _ -> code#dcmpl;
			end;
			CmpNormal(op,TDouble)
		| [TFloat;TFloat] ->
			let op = flip_cmp_op op in
			begin match op with
			| CmpGe | CmpGt -> code#fcmpg;
			| _ -> code#fcmpl;
			end;
			CmpNormal(op,TFloat)
		| [TLong;TLong] ->
			let op = flip_cmp_op op in
			code#lcmpl;
			CmpNormal(op,TLong)
		| [t1;t2] ->
			jerror (Printf.sprintf "Can't compare %s and %s" (generate_signature false t1) (generate_signature false t2))
		| tl ->
			jerror (Printf.sprintf "Bad stack: %s" (String.concat ", " (List.map (generate_signature false) tl)));

	method binop_compare op e1 e2 =
		let sig1 = jsignature_of_type gctx e1.etype in
		let sig2 = jsignature_of_type gctx e2.etype in
		match (Texpr.skip e1),(Texpr.skip e2) with
		| {eexpr = TConst TNull},_ when not (is_unboxed sig2) ->
			self#texpr rvalue_any e2;
			CmpSpecial ((if op = CmpEq then jm#get_code#if_nonnull else jm#get_code#if_null) sig2)
		| _,{eexpr = TConst TNull} when not (is_unboxed sig1) ->
			self#texpr rvalue_any e1;
			CmpSpecial ((if op = CmpEq then jm#get_code#if_nonnull else jm#get_code#if_null) sig1)
		| {eexpr = TConst (TInt i32);etype = t2},e1 when Int32.to_int i32 = 0 && sig2 = TInt ->
			let op = match op with
				| CmpGt -> CmpGe
				| CmpLt -> CmpLe
				| CmpLe -> CmpLt
				| CmpGe -> CmpGt
				| CmpEq -> CmpNe
				| CmpNe -> CmpEq
			in
			self#texpr rvalue_any e1;
			self#cast t2;
			CmpNormal(op,TInt)
		| e1,{eexpr = TConst (TInt i32); etype = t2;} when Int32.to_int i32 = 0 && sig1 = TInt->
			let op = flip_cmp_op op in
			self#texpr rvalue_any e1;
			self#cast t2;
			CmpNormal(op,TInt)
		| _ ->
			match is_unboxed sig1,is_unboxed sig2 with
			| true,true ->
				let f e ret = self#texpr ret e in
				self#binop_exprs (self#get_binop_type e1.etype e2.etype) (f e1) (f e2);
				self#do_compare op
			| false,false ->
				let sig_unboxed1 = get_unboxed_type sig1 in
				let sig_unboxed2 = get_unboxed_type sig2 in
				if sig1 = sig_unboxed1 && sig2 = sig_unboxed2 then begin
					(* No basic types involved, do normal comparison *)
					self#texpr rvalue_any e1;
					self#texpr rvalue_any e2;
					self#do_compare op
				end else begin
					(* At least one of the types is a wrapped numeric one *)
					let cast_type = self#get_binop_type_sig sig_unboxed1 sig_unboxed2 in
					self#texpr rvalue_any e1;
					jm#get_code#dup;
					jm#if_then_else
						(jm#get_code#if_nonnull sig1)
						(fun () ->
							jm#get_code#pop;
							self#texpr rvalue_any e2;
							match op with
							| CmpEq | CmpGe | CmpLe ->
								self#boolop (CmpSpecial (jm#get_code#if_nonnull sig2))
							| CmpNe ->
								self#boolop (CmpSpecial (jm#get_code#if_null sig2))
							| _ ->
								jm#get_code#pop;
								jm#get_code#bconst false
						)
						(fun () ->
							jm#cast ~not_null:true cast_type;
							self#texpr rvalue_any e2;
							jm#get_code#dup;
							jm#if_then_else
								(jm#get_code#if_nonnull sig2)
								(fun () ->
									jm#get_code#pop;
									jm#get_code#pop;
									jm#get_code#bconst (op = CmpNe);
								)
								(fun () ->
									jm#cast ~not_null:true cast_type;
									self#boolop (self#do_compare op);
								)
						);
					CmpNormal(CmpEq,TBool)
				end
			| false,true ->
				self#texpr rvalue_any e1;
				jm#get_code#dup;
				jm#if_then_else
					(jm#get_code#if_nonnull sig1)
					(fun () ->
						jm#get_code#pop;
						jm#get_code#bconst (op = CmpNe)
					)
					(fun () ->
						(match (get_unboxed_type sig1), sig2 with
						| (TFloat | TDouble as unboxed_sig1), TInt ->
							jm#cast ~not_null:true unboxed_sig1;
							self#texpr rvalue_any e2;
							jm#cast ~not_null:true unboxed_sig1
						| _ ->
							jm#cast ~not_null:true sig2;
							self#texpr rvalue_any e2
						);
						self#boolop (self#do_compare op)
					);
				CmpNormal(CmpEq,TBool)
			| true,false ->
				self#texpr rvalue_any e1;
				let cast =
					match sig1, (get_unboxed_type sig2) with
					| TInt, (TFloat | TDouble as unboxed_sig2) ->
						jm#cast ~not_null:true unboxed_sig2;
						self#texpr rvalue_any e2;
						(fun() -> jm#cast ~not_null:true unboxed_sig2)
					| _ ->
						self#texpr rvalue_any e2;
						(fun() -> jm#cast ~not_null:true sig1)
				in
				jm#get_code#dup;
				jm#if_then_else
					(jm#get_code#if_nonnull sig2)
					(fun () ->
						jm#get_code#pop;
						jm#get_code#pop;
						jm#get_code#bconst (op = CmpNe);
					)
					(fun () ->
						cast();
						self#boolop (self#do_compare op)
					);
				CmpNormal(CmpEq,TBool)

	method binop_basic ret op cast_type f1 f2 =
		let emit_exprs () = self#binop_exprs cast_type f1 f2 in
		let method_name () = match op with
			| OpAdd -> "opAdd"
			| OpSub -> "opSub"
			| OpMult -> "opMul"
			| OpDiv -> "opDiv"
			| OpMod -> "opMod"
			| OpAnd -> "opAnd"
			| OpOr -> "opOr"
			| OpXor -> "opXor"
			| OpShl -> "opShl"
			| OpShr -> "opShr"
			| OpUShr -> "opUshr"
			| _ -> die "" __LOC__
		in
		begin match cast_type with
			| TByte | TShort | TInt ->
				begin match op with
				| OpAdd ->
					emit_exprs();
					code#iadd
				| OpSub ->
					emit_exprs();
					code#isub
				| OpMult ->
					emit_exprs();
					code#imul
				| OpDiv ->
					f1 (rvalue_sig TDouble);
					jm#cast TDouble;
					f2 (rvalue_sig TDouble);
					jm#cast TDouble;
					code#ddiv;
				| OpAnd ->
					emit_exprs();
					code#iand
				| OpOr ->
					emit_exprs();
					code#ior
				| OpXor ->
					emit_exprs();
					code#ixor
				| OpShl ->
					emit_exprs();
					code#ishl
				| OpShr ->
					emit_exprs();
					code#ishr
				| OpUShr ->
					emit_exprs();
					code#iushr
				| OpMod ->
					emit_exprs();
					code#irem
				| _ -> jerror (Printf.sprintf "Unsupported binop on TInt: %s" (s_binop op))
				end
			| TFloat ->
				emit_exprs();
				begin match op with
				| OpAdd -> code#fadd
				| OpSub -> code#fsub
				| OpMult -> code#fmul
				| OpDiv -> code#fdiv
				| OpMod -> code#frem
				| _ -> jerror (Printf.sprintf "Unsupported binop on TFloat: %s" (s_binop op))
				end
			| TDouble ->
				emit_exprs();
				begin match op with
				| OpAdd -> code#dadd
				| OpSub -> code#dsub
				| OpMult -> code#dmul
				| OpDiv -> code#ddiv
				| OpMod -> code#drem
				| _ -> jerror (Printf.sprintf "Unsupported binop on TDouble: %s" (s_binop op))
				end
			| TLong ->
				begin match op with
				| OpAdd ->
					emit_exprs();
					code#ladd
				| OpSub ->
					emit_exprs();
					code#lsub
				| OpMult ->
					emit_exprs();
					code#lmul
				| OpDiv ->
					emit_exprs();
					code#ldiv
				| OpAnd ->
					emit_exprs();
					code#land_
				| OpOr ->
					emit_exprs();
					code#lor_
				| OpXor ->
					emit_exprs();
					code#lxor_
				| OpShl ->
					f1 (rvalue_sig TLong);
					jm#cast TLong;
					f2 (rvalue_sig TLong);
					jm#cast TInt;
					code#lshl;
				| OpShr ->
					f1 (rvalue_sig TLong);
					jm#cast TLong;
					f2 (rvalue_sig TInt);
					jm#cast TInt;
					code#lshr;
				| OpUShr ->
					f1 (rvalue_sig TLong);
					jm#cast TLong;
					f2 (rvalue_sig TInt);
					jm#cast TInt;
					code#lushr;
				| OpMod ->
					emit_exprs();
					code#lrem
				| _ -> jerror (Printf.sprintf "Unsupported binop on TInt: %s" (s_binop op))
				end
			| TBool | TObject((["java";"lang"],"Object"),_) | TTypeParameter _ ->
				begin match op with
				| OpBoolAnd ->
					let operand f =
						f (rvalue_sig TBool);
						jm#cast TBool;
					in
					operand f1;
					jm#if_then_else
						(code#if_ CmpEq)
						(fun () -> operand f2)
						(fun () -> code#bconst false)
				| OpBoolOr ->
					let operand f =
						f (rvalue_sig TBool);
						jm#cast TBool;
					in
					operand f1;
					jm#if_then_else
						(code#if_ CmpEq)
						(fun () -> code#bconst true)
						(fun () -> operand f2)
				| _ ->
					emit_exprs();
					let name = method_name () in
					jm#invokestatic haxe_jvm_path name (method_sig [object_sig;object_sig] (Some object_sig))
				end
			| TObject(path,_) ->
				if path = string_path then begin
					f1 rvalue_any;
					jm#cast ~allow_to_string:true cast_type;
					f2 rvalue_any;
					jm#cast ~allow_to_string:true cast_type;
					jm#invokestatic haxe_jvm_path "stringConcat" (method_sig [object_sig;object_sig] (Some string_sig))
				end else begin
					emit_exprs();
					let name = method_name () in
					jm#invokestatic haxe_jvm_path name (method_sig [object_sig;object_sig] (Some object_sig))
				end
			| _ ->
				jerror (Printf.sprintf "Unsupported operation %s on %s" (s_binop op) (generate_signature false cast_type))
		end;

	method boolop cmp =
		jm#if_then_else
			(self#apply_cmp cmp)
			(fun () -> code#bconst true)
			(fun () -> code#bconst false)

	method var_slot_is_in_int8_range v =
		let slot,_,_ = self#get_local v in
		in_range true Int8Range slot

	method binop ret op e1 e2 = match op,ret with
		| (OpEq | OpNotEq | OpLt | OpGt | OpLte | OpGte),_ ->
			let op = convert_cmp_op op in
			self#boolop (self#binop_compare op e1 e2)
		| OpAssign,_ ->
			let f () =
				self#texpr (rvalue_type gctx e1.etype) e2;
				self#cast e1.etype;
			in
			self#read_write ret AKNone e1 f
		| OpAssignOp op,_ ->
			let jsig1 = jsignature_of_type gctx e1.etype in
			begin match op,(Texpr.skip e1).eexpr,(Texpr.skip e2).eexpr with
			| OpAdd,TLocal v,TConst (TInt i32) when is_really_int v.v_type && in_range false Int8Range (Int32.to_int i32) && self#var_slot_is_in_int8_range v->
				let slot,load,_ = self#get_local v in
				let i = Int32.to_int i32 in
				code#iinc slot i;
				if need_val ret then load();
			| OpSub,TLocal v,TConst (TInt i32) when is_really_int v.v_type && in_range false Int8Range (-Int32.to_int i32) && self#var_slot_is_in_int8_range v ->
				let slot,load,_ = self#get_local v in
				let i = -Int32.to_int i32 in
				code#iinc slot i;
				if need_val ret then load();
			| _ ->
				let f () =
					self#binop_basic ret op (self#get_binop_type e1.etype e2.etype) (fun _ -> ()) (fun ret -> self#texpr ret e2);
					jm#cast jsig1;
				in
				self#read_write ret AKPre e1 f
			end
		| _ ->
			let f e ret = self#texpr ret e in
			self#binop_basic ret op (self#get_binop_type e1.etype e2.etype) (f e1) (f e2)

	method unop ret op flag e =
		match op,(Texpr.skip e).eexpr with
		| (Increment | Decrement),TLocal v when is_really_int v.v_type && self#var_slot_is_in_int8_range v ->
			let slot,load,_ = self#get_local v in
			if flag = Postfix && need_val ret then load();
			code#iinc slot (if op = Increment then 1 else -1);
			if flag = Prefix && need_val ret then load();
		| (Increment | Decrement),_ ->
			let is_null = is_null e.etype in
			let f () =
				begin match jm#get_code#get_stack#top with
				| TLong ->
					code#lconst Int64.one;
					if op = Increment then code#ladd else code#lsub
				| TDouble ->
					code#dconst 1.;
					if op = Increment then code#dadd else code#dsub
				| TByte | TShort | TInt ->
					code#iconst Int32.one;
					if op = Increment then code#iadd else code#isub;
					if is_null then self#expect_reference_type;
				| _ ->
					jm#invokestatic haxe_jvm_path (if op = Increment then "opIncrement" else "opDecrement") (method_sig [object_sig] (Some object_sig))
				end
			in
			self#read_write ret (if flag = Prefix then AKPre else AKPost) e f;
		| Neg,_ ->
			self#texpr rvalue_any e;
			let jsig = jsignature_of_type gctx (follow e.etype) in
			jm#cast jsig;
			begin match jsig with
			| TLong -> code#lneg;
			| TDouble -> code#dneg;
			| TByte | TShort | TInt -> code#ineg;
			| _ -> jm#invokestatic haxe_jvm_path "opNeg" (method_sig [object_sig] (Some object_sig))
			end;
			self#cast e.etype;
		| Not,_ ->
			jm#if_then_else_labeled
				(self#condition false e)
				(fun () -> code#bconst false)
				(fun () -> code#bconst true)
		| Spread, _ ->
			self#texpr (rvalue_type gctx e.etype) e
		| NegBits,_ ->
			let jsig = jsignature_of_type gctx (follow e.etype) in
			self#texpr rvalue_any e;
			jm#cast jsig;
			begin match jsig with
			| TByte | TShort | TInt ->
				code#iconst Int32.minus_one;
				code#ixor;
			| TLong ->
				code#lconst Int64.minus_one;
				code#lxor_;
			| _ ->
				jm#invokestatic haxe_jvm_path "opNegBits" (method_sig [object_sig] (Some object_sig))
			end;
			self#cast e.etype;

	(* calls *)

	method call_arguments ?(cast=true) t el =
		let tl,tr = match follow t with
			| TFun(tl,tr) ->
				tl,return_of_type gctx tr
			| _ ->
				List.map (fun e -> ("",false,e.etype)) el,Some (object_sig)
		in
		let rec loop acc tl el = match tl,el with
			| (_,_,t) :: tl,e :: el ->
				let jsig = jsignature_of_type gctx t in
				begin match tl,Type.follow t with
				| [],(TAbstract({a_path = ["haxe"],"Rest"},[t1])) ->
					(match e.eexpr with
					| TUnop (Spread,_,e) ->
						self#texpr (rvalue_sig jsig) e
					| _ ->
						self#new_native_array (get_boxed_type (jsignature_of_type gctx t1)) (e :: el)
					);
					List.rev (jsig :: acc)
				| _ ->
					self#texpr (rvalue_sig jsig) e;
					let acc = if cast then begin
						jm#cast jsig;
						jsig :: acc
					end else
						code#get_stack#top :: acc
					in
					loop acc tl el
				end
			| [(_,_,t)],[] ->
				(match Type.follow t with
				| TAbstract({a_path = ["haxe"],"Rest"},[t1]) ->
					let jsig = jsignature_of_type gctx t in
					self#new_native_array (get_boxed_type (jsignature_of_type gctx t1)) [];
					List.rev (jsig :: acc)
				| _ -> List.rev acc)
			| _,[] -> List.rev acc
			| [],e :: el ->
				(* TODO: this sucks *)
				self#texpr rvalue_any e;
				loop (self#vtype e.etype :: acc) [] el
		in
		let tl = loop [] tl el in
		tl,tr

	method call ret tr e1 el =
		let invoke t =
			jm#cast haxe_function_sig;
			(* We don't want to cast because typed functions handle that for us. *)
			let tl,tr = self#call_arguments ~cast:false t el in
			let meth = gctx.typed_functions#register_signature tl tr in
			jm#invokevirtual haxe_function_path meth.name (method_sig meth.dargs meth.dret);
			tr
		in
		let tro = match (Texpr.skip e1).eexpr with
		| TField(_,FStatic({cl_path = ["haxe";"jvm"],"Jvm"},({cf_name = "referenceEquals"} as cf))) ->
			let tl,tr = self#call_arguments cf.cf_type el in
			begin match tl with
				| [t1;t2] -> self#boolop (CmpSpecial (code#if_acmp_ne t1 t2))
				| _ -> die "" __LOC__
			end;
			tr
		| TField(_,FStatic({cl_path = ["haxe";"jvm"],"Jvm"},({cf_name = "instanceof"}))) ->
			begin match el with
				| [e1;{eexpr = TTypeExpr mt;epos = pe}] ->
					self#texpr rvalue_any e1;
					self#expect_reference_type;
					let path = match jsignature_of_type gctx (type_of_module_type mt) with
						| TObject(path,_) -> path
						| _ -> Error.typing_error "Class expected" pe
					in
					code#instanceof path;
					Some TBool
				| _ -> Error.typing_error "Type expression expected" e1.epos
			end;
		| TField(_,FStatic({cl_path = (["java";"lang"],"Math")},{cf_name = ("isNaN" | "isFinite") as name})) ->
			begin match el with
			| [e1] ->
				self#texpr rvalue_any e1;
				jm#cast TDouble;
				jm#invokestatic (["java";"lang"],"Double") name (method_sig [TDouble] (Some TBool));
				Some TBool
			| _ ->
				die "" __LOC__
			end;
		| TField(_,FStatic({cl_path = (["java";"lang"],"Math")},{cf_name = ("floor" | "ceil" | "round") as name})) ->
			begin match el with
			| [e1] ->
				self#texpr rvalue_any e1;
				jm#cast TDouble;
				let rsig = if name = "round" then TLong else TDouble in
				jm#invokestatic (["java";"lang"],"Math") name (method_sig [TDouble] (Some rsig));
				jm#cast TInt;
				Some TInt
			| _ ->
				die "" __LOC__
			end;
		| TField(_,FStatic({cl_path = (["java";"lang"],"Math")} as c,({cf_name = ("ffloor" | "fceil")} as cf))) ->
			let tl,tr = self#call_arguments cf.cf_type el in
			jm#invokestatic c.cl_path (String.sub cf.cf_name 1 (String.length cf.cf_name - 1)) (method_sig tl tr);
			tr
		| TField(_,FStatic({cl_path = (["haxe"],"Int64$Int64_Impl_")},{cf_name = "make"})) ->
			begin match el with
			| [{eexpr = TConst (TInt i1)};{eexpr = TConst (TInt i2)}] ->
				let high = Int64.of_int32 i1 in
				let high = Int64.shift_left high 32 in
				let low = Int64.of_int32 i2 in
				let low = Int64.logand low (Int64.of_string "0xFFFFFFFF") in
				let i = Int64.logor high low in
				jm#get_code#lconst i;
				Some TLong
			| [e1;e2] ->
				self#texpr (rvalue_sig TLong) e1;
				jm#cast TLong;
				jm#get_code#iconst (Int32.of_int 32);
				jm#get_code#lshl;
				self#texpr (rvalue_sig TLong) e2;
				jm#cast TLong;
				jm#get_code#lconst (Int64.of_string "0xFFFFFFFF");
				jm#get_code#land_;
				jm#get_code#lor_;
				Some TLong
			| _ ->
				die "" __LOC__
			end
		| TIdent "__array__" | TField(_,FStatic({cl_path = (["java"],"NativeArray")},{cf_name = "make"})) ->
			begin match follow tr with
			| TInst({cl_path = (["java"],"NativeArray")},[t]) ->
				let jsig = self#vtype t in
				self#new_native_array jsig el;
				Some (array_sig jsig)
			| _ ->
				Error.typing_error (Printf.sprintf "Bad __array__ type: %s" (s_type (print_context()) tr)) e1.epos;
			end
		| TField(_,FStatic({cl_path = (["haxe"],"Rest$Rest_Impl_")},{cf_name = "createNative"})) ->
			begin match tr, el with
			| TType({ t_path = ["haxe"],"Rest$NativeRest" },[t]), [e2] ->
				self#texpr (if need_val ret then rvalue_any else RVoid) e2;
				let jsig = get_boxed_type (self#vtype t) in
				ignore(NativeArray.create jm#get_code jc#get_pool jsig);
				Some (array_sig jsig)
			| _ ->
				Error.typing_error (Printf.sprintf "Bad __array__ type: %s" (s_type (print_context()) tr)) e1.epos;
			end
		| TField(e1,FStatic(c,({cf_kind = Method (MethNormal | MethInline)} as cf))) ->
			let tl,tr = self#call_arguments cf.cf_type el in
			jm#invokestatic c.cl_path cf.cf_name (method_sig tl tr);
			tr
		| TField(e1,FInstance({cl_path=(["haxe";"root"],"StringBuf");cl_descendants=[]} as c,_,({cf_name="add"} as cf))) ->
			self#texpr rvalue_any e1;
			let jsig = match el with
			| [ea1] ->
				self#texpr rvalue_any ea1;
				begin match code#get_stack#top with
				| TBool | TChar | TDouble | TFloat | TInt | TLong | TObject((["java";"lang"],"String"),_) as jsig ->
					jsig
				| TByte | TShort ->
					jm#cast TInt;
					TInt
				| _ ->
					jm#cast object_sig;
					object_sig
				end;
			| _ ->
				ignore(self#call_arguments cf.cf_type el);
				object_sig
			in
			jm#invokevirtual c.cl_path "add" (method_sig [jsig] None);
			None
		| TField(e1,FInstance(c,tl,({cf_kind = Method (MethNormal | MethInline)} as cf))) ->
			let is_super = match e1.eexpr with
			| TConst TSuper ->
				code#aload jc#get_jsig 0;
				true
			| _ ->
				self#texpr rvalue_any e1;
				false
			in
			let tl,tr = self#call_arguments cf.cf_type el in
			(if is_super then jm#invokespecial else if (has_class_flag c CInterface) then jm#invokeinterface else jm#invokevirtual) c.cl_path cf.cf_name (method_sig tl tr);
			tr
		| TField(_,FEnum(en,ef)) ->
			let tl,_ = self#call_arguments ef.ef_type el in
			let tr = self#vtype tr in
			jm#invokestatic en.e_path ef.ef_name (method_sig tl (Some tr));
			Some tr
		| TField(e11,FAnon cf) ->
			begin match gctx.anon_identification#identify false e11.etype with
			| Some {pfm_path=path_anon} ->
				begin match gctx.typedef_interfaces#get_interface_class path_anon with
				| Some c ->
					let c,_,cf = raw_class_field (fun cf -> cf.cf_type) c [] cf.cf_name in
					let path_inner = match c with
						| Some(c,_) -> c.cl_path
						| _ -> die "" __LOC__
					in
					self#texpr rvalue_any e11;
					let tl,tr = self#call_arguments cf.cf_type el in
					jm#invokeinterface path_inner cf.cf_name (self#vtype cf.cf_type);
					Option.may jm#cast tr;
					tr
				| None ->
					self#texpr rvalue_any e1;
					invoke e1.etype
				end
			| None ->
				self#texpr rvalue_any e1;
				invoke e1.etype
			end
		| TConst TSuper ->
			let c,cf = match gctx.current_field_info with
				| Some ({super_call_fields = hd :: tl} as info) ->
					info.super_call_fields <- tl;
					hd
				| _ ->
					Error.typing_error "Something went wrong" e1.epos
			in
			let kind = get_construction_mode c cf in
			begin match kind with
				| ConstructInitPlusNew when jm#get_name = "<init>" ->
					jm#load_this;
					jm#get_code#aconst_null haxe_empty_constructor_sig;
					jm#call_super_ctor ConstructInit (method_sig [haxe_empty_constructor_sig] None);
				| _ ->
					()
			end;
			jm#load_this;
			let tl,_ = self#call_arguments cf.cf_type el in
			jm#call_super_ctor kind (method_sig tl None);
			None
		| TIdent "__lock__" ->
			begin match el with
				| [e1;e2] ->
					self#texpr rvalue_any e1;
					jm#get_code#dup;
					let _,load,save = jm#add_local "tmp" (self#vtype e1.etype) VarWillInit in
					save();
					jm#get_code#monitorenter;
					let f_exit () =
						load();
						jm#get_code#monitorexit;
					in
					block_exits <- (ExitExecute f_exit) :: block_exits;
					let fp_from = jm#get_code#get_fp in
					self#texpr RVoid e2;
					let term_try = jm#is_terminated in
					if not jm#is_terminated then f_exit();
					let fp_to = jm#get_code#get_fp in
					let r_try = jm#maybe_make_jump in
					let fp_target = jm#get_code#get_fp in
					let pop_scope = jm#push_scope in
					code#get_stack#push throwable_sig;
					jm#add_stack_frame;
					jm#add_exception {
						exc_start_pc = fp_from;
						exc_end_pc = fp_to;
						exc_handler_pc = fp_target;
						exc_catch_type = None;
					};
					begin
						let _,load,save = jm#add_local "tmp" throwable_sig VarWillInit in
						save();
						f_exit();
						jm#add_exception {
							exc_start_pc = fp_target;
							exc_end_pc = jm#get_code#get_fp;
							exc_handler_pc = fp_target;
							exc_catch_type = None;
						};
						load();
						jm#get_code#athrow;
					end;
					pop_scope();
					block_exits <- List.tl block_exits;
					if not term_try then jm#add_stack_frame;
					jm#close_jumps true ([term_try,r_try]);
					None
				| _ -> die "" __LOC__
			end
		| _ ->
			self#texpr rvalue_any e1;
			invoke e1.etype;
		in
		match need_val ret,tro with
		| false,Some _ -> code#pop
		| false,None -> ()
		| true,Some _ -> self#cast tr;
		| true,None -> die "" __LOC__

	(* exceptions *)

	method try_catch ret e1 catches =
		let restore = jm#start_branch in
		let fp_from = code#get_fp in
		let old_exceptions = caught_exceptions in
		let excl = List.map (fun (v,e) ->
			let exc = new haxe_exception gctx v.v_type in
			caught_exceptions <- exc :: caught_exceptions;
			exc,v,e
		) catches in
		self#texpr ret e1;
		caught_exceptions <- old_exceptions;
		let term_try = jm#is_terminated in
		let r_try = jm#maybe_make_jump in
		let fp_to = code#get_fp in
		let start_exception_block path jsig =
			restore();
			let fp_target = code#get_fp in
			let offset = pool#add_path path in
			jm#add_exception {
				exc_start_pc = fp_from;
				exc_end_pc = fp_to;
				exc_handler_pc = fp_target;
				exc_catch_type = Some offset;
			};
			code#get_stack#push jsig;
			jm#add_stack_frame;
		in
		let run_catch_expr v e =
			let pop_scope = jm#push_scope in
			let _,_,store = self#add_local v VarWillInit in
			store();
			self#texpr ret e;
			pop_scope();
			jm#is_terminated
		in
		let add_catch (exc,v,e) =
			start_exception_block exc#get_native_path exc#get_native_type;
			let term = run_catch_expr v e in
			let r = jm#maybe_make_jump in
			term,r
		in
		let rec loop acc excl = match excl with
			| (exc,v,e) :: excl ->
				let res = add_catch (exc,v,e) in
				loop (res :: acc) excl
			| [] ->
				acc
		in
		let rl = loop [] excl in
		jm#close_jumps true ((term_try,r_try) :: rl)

	method emit_block_exits is_loop_exit =
		let rec loop stack = match stack with
			| [] ->
				()
			| ExitLoop :: stack ->
				if not is_loop_exit then loop stack
			| ExitExecute f :: stack ->
				f();
				loop stack
		in
		loop block_exits

	(* texpr *)

	method const ret t ct = match ct with
		| Type.TInt i32 ->
			begin match ret with
			| RValue (Some (TLong | TObject((["java";"lang"],"Long"),_))) -> code#lconst (Int64.of_int32 i32)
			| RValue (Some (TDouble | TObject((["java";"lang"],"Double"),_))) -> code#dconst (Int32.to_float i32)
			| _ -> code#iconst i32
			end
		| TFloat f ->
			begin match ret with
			| RValue (Some (TFloat | TObject((["java";"lang"],"Float"),_))) -> code#fconst (float_of_string f)
			| _ -> code#dconst (float_of_string f)
			end
		| TBool true -> code#bconst true
		| TBool false -> code#bconst false
		| TNull -> jm#load_default_value (self#vtype t)
		| TThis ->
			let _,load,_ = self#get_local_by_id (0,"this") in
			load()
		| TString s -> jm#string s
		| TSuper -> failwith "Invalid super access"

	method new_native_array jsig el =
		jm#new_native_array jsig (List.map (fun e -> fun () -> self#texpr (rvalue_sig jsig) e) el)

	method spawn_compiled_pattern_field (s1 : string) (s2 : string) =
		let name = Printf.sprintf "_hx_pattern_%s_%i" (patch_name jm#get_name) jm#get_next_regex_id in
		let jf = jc#spawn_field name NativeSignatures.haxe_compiled_pattern_sig [FdStatic;FdPrivate;FdFinal] in
		let jm = jc#get_static_init_method in
		jm#string s1;
		jm#string s2;
		jm#invokestatic NativeSignatures.haxe_ereg_path "compilePattern" (method_sig [string_sig;string_sig] (Some NativeSignatures.haxe_compiled_pattern_sig));
		jm#putstatic jc#get_this_path jf#get_name jf#get_jsig;
		jf

	method texpr ret e =
		if not jm#is_terminated then self#texpr' ret e

	method texpr' ret e =
		code#set_line (Lexer.get_error_line e.epos);
		match e.eexpr with
		| TVar(v,Some e1) ->
			self#texpr (rvalue_type gctx v.v_type) e1;
			self#cast v.v_type;
			let _,_,store = self#add_local v VarWillInit in
			store()
		| TVar(v,None) ->
			ignore(self#add_local v VarNeedDefault);
		| TLocal _ | TConst _  | TTypeExpr _ when not (need_val ret) ->
			()
		| TLocal v ->
			let _,load,_ = self#get_local v in
			load()
		| TTypeExpr mt ->
			let t = type_of_module_type mt in
			if ExtType.is_void (follow t) then jm#get_basic_type_class "Void"
			else jm#get_class (jsignature_of_type gctx t)
		| TUnop(op,flag,e1) ->
			begin match op with
			| Not | Neg | NegBits when not (need_val ret) -> self#texpr ret e1
			| _ -> self#unop ret op flag e1
			end
		| TBinop(OpAdd,e1,e2) when ExtType.is_string (follow e.etype) ->
			let string_builder_path = (["java";"lang"],"StringBuilder") in
			let string_builder_sig = object_path_sig string_builder_path in
			jm#construct ConstructInit string_builder_path (fun () -> []);
			let rec loop e = match e.eexpr with
				| TBinop(OpAdd,e1,e2) when ExtType.is_string (follow e.etype) ->
					loop e1;
					loop e2;
				| _ ->
					self#texpr rvalue_any e;
					let jsig = jm#get_code#get_stack#top in
					let jsig = match jsig with
						| TByte | TBool | TChar | TShort | TInt | TLong ->
							jsig
						| TFloat | TDouble ->
							(* Haxe mandates 2.0 be printed as 2, so we have to wrap here... *)
							jm#expect_reference_type;
							jm#invokestatic haxe_jvm_path "toString" (method_sig [object_sig] (Some string_sig));
							string_sig
						| _ ->
							object_sig
					in
					jm#invokevirtual string_builder_path "append" (method_sig [jsig] (Some string_builder_sig));
			in
			loop e1;
			loop e2;
			jm#invokevirtual string_builder_path "toString" (method_sig [] (Some string_sig));
		| TBinop(op,e1,e2) ->
			begin match op with
			| OpAssign | OpAssignOp _ -> self#binop ret op e1 e2
			| _ when not (need_val ret) ->
				self#texpr ret e1;
				self#texpr ret e2;
			| _ ->
				self#binop ret op e1 e2
			end
		| TConst ct ->
			self#const ret e.etype ct
		| TIf(e1,e2,None) ->
			jm#if_then_labeled
				(self#condition false e1)
				(fun () -> self#texpr RVoid (mk_block e2));
		| TIf(e1,e2,Some e3) ->
			jm#if_then_else_labeled
				(self#condition false e1)
				(fun () ->
					self#texpr ret (mk_block e2);
					if need_val ret then self#cast e.etype
				)
				(fun () ->
					self#texpr ret (mk_block e3);
					if need_val ret then self#cast e.etype;
				)
		| TSwitch(e1,cases,def) ->
			self#switch ret e1 cases def
		| TWhile(e1,e2,flag) -> (* TODO: do-while *)
			block_exits <- ExitLoop :: block_exits;
			let is_true_loop = match (Texpr.skip e1).eexpr with TConst (TBool true) -> true | _ -> false in
			let continue_label = jm#spawn_label "continue" in
			let break_label = jm#spawn_label "break" in
			let body_label = jm#spawn_label "body" in
			let old_continue = continue in
			continue <- Some continue_label;
			let old_break = break in
			break <- Some break_label;
			continue_label#here;
			let restore = jm#start_branch in
			if not is_true_loop then self#condition false e1 body_label break_label;
			let pop_scope = jm#push_scope in
			body_label#here;
			self#texpr RVoid e2;
			if not jm#is_terminated then continue_label#goto;
			pop_scope();
			restore();
			if break_label#was_jumped_to || not is_true_loop then
				break_label#here
			else
				jm#set_terminated true;
			continue <- old_continue;
			break <- old_break;
			block_exits <- List.tl block_exits;
		| TBreak ->
			self#emit_block_exits true;
			begin match break with
			| None ->
				jerror "break outside loop"
			| Some label ->
				label#goto;
			end;
		| TContinue ->
			self#emit_block_exits true;
			begin match continue with
			| None ->
				jerror "continue outside loop"
			| Some label ->
				label#goto;
			end;
		| TTry(e1,catches) ->
			self#try_catch ret e1 catches
		| TField(e1,fa) ->
			if not (need_val ret) then self#texpr ret e1
			else self#read (fun () -> self#cast_expect ret e.etype) e1 fa;
		| TCall(e1,el) ->
			self#call ret e.etype e1 el
		| TNew({cl_path = (["java"],"NativeArray")},[t],[e1]) ->
			self#texpr (if need_val ret then rvalue_any else RVoid) e1;
			(* Technically this could throw... but whatever *)
			if need_val ret then ignore(NativeArray.create jm#get_code jc#get_pool (jsignature_of_type gctx t))
		| TNew({cl_path=(["haxe";"root"],"EReg") as ereg_path},[],[{eexpr = TConst (TString s1)};{eexpr = TConst (TString s2)}]) when jm != jc#get_static_init_method ->
			let jf = self#spawn_compiled_pattern_field s1 s2 in
			jm#construct ConstructInit ereg_path (fun () ->
				jm#getstatic jc#get_this_path jf#get_name jf#get_jsig;
				[jf#get_jsig]
			)
		| TNew(c,tl,el) ->
			begin match OverloadResolution.maybe_resolve_constructor_overload c tl el with
			| None -> Error.typing_error "Could not find overload" e.epos
			| Some (c',cf,_) ->
				let f () =
					let tl,_ = self#call_arguments cf.cf_type el in
					tl
				in
				jm#construct ~no_value:(if not (need_val ret) then true else false) (get_construction_mode c' cf) c.cl_path f
			end
		| TReturn None ->
			self#emit_block_exits false;
			jm#return;
		| TReturn (Some e1) ->
			self#texpr rvalue_any e1;
			let jsig = Option.get return_type in
			jm#cast jsig;
			self#emit_block_exits false;
			jm#return;
		| TFunction tf ->
			self#tfunction e tf
		| TArrayDecl el when not (need_val ret) ->
			List.iter (self#texpr ret) el
		| TArrayDecl el ->
			begin match follow e.etype with
			| TInst({cl_path = (["haxe";"root"],"Array")},[t]) ->
				self#new_native_array (jsignature_of_type gctx (self#mknull t)) el;
				jm#invokestatic (["haxe";"root"],"Array") "ofNative" (method_sig [array_sig object_sig] (Some (object_path_sig (["haxe";"root"],"Array"))));
				self#cast e.etype
			| _ ->
				die "" __LOC__
			end
		| TArray(e1,e2) when not (need_val ret) ->
			(* Array access never throws so this should be fine... *)
			self#texpr ret e1;
			self#texpr ret e2;
		| TArray(e1,e2) ->
			begin match follow e1.etype with
			| TInst({cl_path = (["haxe";"root"],"Array")} as c,[t]) ->
				self#texpr rvalue_any e1;
				self#texpr (rvalue_sig TInt) e2;
				jm#cast TInt;
				jm#invokevirtual c.cl_path "__get" (method_sig [TInt] (Some object_sig));
				self#cast e.etype
			| TInst({cl_path = (["java"],"NativeArray")},[t]) ->
				self#texpr rvalue_any e1;
				let vt = self#vtype e1.etype in
				let vte =
					let vte = self#vtype t in
					match e1.etype with
					| TType ({ t_path = ["haxe"],"Rest$NativeRest" },_) -> get_boxed_type vte
					| _ -> vte
				in
				self#texpr rvalue_any e2;
				self#read_native_array vt vte
			| t ->
				self#texpr rvalue_any e1;
				self#texpr rvalue_any e2;
				jm#cast TInt;
				jm#invokestatic (["haxe";"jvm"],"Jvm") "arrayRead" (method_sig [object_sig;TInt] (Some object_sig));
				self#cast e.etype;
			end
		| TBlock [] ->
			if ret = RReturn && not jm#is_terminated then jm#return;
		| TBlock el ->
			let rec loop el = match el with
				| [] -> die "" __LOC__
				| [e1] ->
					self#texpr ret e1;
					if ret = RReturn && not jm#is_terminated then jm#return;
				| e1 :: el ->
					self#texpr RVoid e1;
					loop el
			in
			let pop_scope = jm#push_scope in
			loop el;
			pop_scope();
		| TCast(e1,None) ->
			self#texpr ret e1;
			if need_val ret then self#cast e.etype
		| TCast(e1,Some mt) ->
			self#texpr rvalue_any e1;
			let jsig = jsignature_of_type gctx (type_of_module_type mt) in
			if is_unboxed jsig || is_unboxed jm#get_code#get_stack#top then jm#cast jsig
			else code#checkcast (t_infos mt).mt_path;
			if not (need_val ret) then code#pop;
		| TParenthesis e1 | TMeta(_,e1) ->
			self#texpr ret e1
		| TFor(v,e1,e2) ->
			self#texpr ret (Texpr.for_remap com.basic v e1 e2 e.epos)
		| TEnumIndex e1 ->
			self#texpr rvalue_any e1;
			jm#invokevirtual java_enum_path "ordinal" (method_sig [] (Some TInt))
		| TEnumParameter(e1,ef,i) ->
			self#texpr rvalue_any e1;
			let path,name,jsig_arg = match follow ef.ef_type with
				| TFun(tl,TEnum(en,_)) ->
					let n,_,t = List.nth tl i in
					en.e_path,n,self#vtype t
				| _ -> die "" __LOC__
			in
			let cpath = ((fst path),Printf.sprintf "%s$%s" (snd path) ef.ef_name) in
			let jsig = (object_path_sig cpath) in
			jm#cast jsig;
			jm#getfield cpath name jsig_arg;
			self#cast e.etype;
		| TThrow e1 ->
			self#texpr rvalue_any e1;
			(* There could be something like `throw throw`, so we should only throw if we aren't terminated (issue #10363) *)
			if not (jm#is_terminated) then begin
				if not (Exceptions.is_haxe_exception e1.etype) && not (does_unify e1.etype gctx.t_runtime_exception) then begin
					let exc = new haxe_exception gctx e1.etype in
					if not (List.exists (fun exc' -> exc#is_assignable_to exc') caught_exceptions) then
						jm#add_thrown_exception exc#get_native_path;
				end;
				code#athrow;
				jm#set_terminated true
			end
		| TObjectDecl fl ->
			let td = gctx.anon_identification#identify true e.etype in
			begin match follow e.etype,td with
			(* The guard is here because in the case of quoted fields like `"a-b"`, the field is not part of the
			   type. In this case we have to do full dynamic construction. *)
			| TAnon an,Some pfm when List.for_all (fun ((name,_,_),_) -> PMap.mem name an.a_fields) fl ->
				let fl' = convert_fields gctx pfm in
				jm#construct ConstructInit pfm.pfm_path (fun () ->
					(* We have to respect declaration order, so let's temp var where necessary *)
					let rec loop fl fl' ok acc = match fl,fl' with
						| ((name,_,_),e) :: fl,(name',jsig) :: fl' ->
							if ok && name = name' then begin
								self#texpr rvalue_any e;
								jm#cast jsig;
								loop fl fl' ok acc
							end else begin
								let load = match (Texpr.skip e).eexpr with
								| TConst _ | TTypeExpr _ | TFunction _ ->
									(fun () -> self#texpr rvalue_any e)
								| _ ->
									let _,load,save = jm#add_local (Printf.sprintf "_hx_tmp_%s" name) (self#vtype e.etype) VarWillInit in
									self#texpr rvalue_any e;
									save();
									load
								in
								loop fl fl' false ((name,load) :: acc)
							end
						| [],[] ->
							acc
						| (_,e) :: fl,[] ->
							self#texpr RVoid e;
							loop fl fl' ok acc
						| [],(_,jsig) :: fl' ->
							jm#load_default_value jsig;
							loop [] fl' ok acc
					in
					let vars = loop fl fl' true [] in
					let vars = List.sort (fun (name1,_) (name2,_) -> compare name1 name2) vars in
					List.iter (fun (name,load) ->
						load();
						if List.mem_assoc name fl' then jm#cast (List.assoc name fl')
					) vars;
					List.map snd fl';
				)
			| _ ->
				jm#construct ConstructInit haxe_dynamic_object_path (fun () -> []);
				List.iter (fun ((name,_,_),e) ->
					code#dup;
					jm#string name;
					self#texpr rvalue_any e;
					self#expect_reference_type;
					jm#invokevirtual haxe_dynamic_object_path "_hx_setField" (method_sig [string_sig;object_sig] None);
				) fl;
			end
		| TIdent _ ->
			Error.typing_error (s_expr_ast false "" (s_type (print_context())) e) e.epos;

	(* api *)

	method object_constructor =
		let path = jc#get_super_path in
		let offset = pool#add_field path "<init>" (method_sig [] None) FKMethod in
		jm#load_this;
		code#invokespecial offset jc#get_jsig [] [];
		jm#set_this_initialized
end

type super_ctor_mode =
	| SCNone
	| SCJava
	| SCHaxe

let generate_dynamic_access gctx (jc : JvmClass.builder) fields is_anon =
	begin match fields with
	| [] ->
		()
	| _ ->
		let jsig = method_sig [string_sig] (Some object_sig) in
		let jm = jc#spawn_method "_hx_getField" jsig [MPublic;MSynthetic] in
		let _,load,_ = jm#add_local "name" string_sig VarArgument in
		jm#finalize_arguments;
		let cases = List.map (fun (name,jsig,kind) ->
			[name],(fun () ->
			begin match kind,jsig with
				| Method (MethNormal | MethInline),TMethod(args,_) ->
					if gctx.dynamic_level >= 2 then begin
						create_field_closure gctx jc jc#get_this_path jm name jsig (fun () -> jm#load_this)
					end else begin
						jm#load_this;
						jm#string name;
						jm#new_native_array java_class_sig (List.map (fun jsig -> fun () -> jm#get_class jsig) args);
						jm#invokestatic haxe_jvm_path "readFieldClosure" (method_sig [object_sig;string_sig;array_sig (java_class_sig)] (Some (object_sig)))
					end
				| _ ->
					jm#load_this;
					jm#getfield jc#get_this_path name jsig;
					jm#expect_reference_type;
				end;
				jm#replace_top object_sig;
			)
		) fields in
		let def = (fun () ->
			jm#load_this;
			load();
			jm#invokespecial jc#get_super_path "_hx_getField" jsig;
		) in
		jm#string_switch true load cases (Some def);
		jm#return
	end;
	let fields = List.filter (fun (_,_,kind) -> match kind with
		| Method (MethNormal | MethInline) -> false
		| Var {v_write = AccNever} -> false
		| _ -> true
	) fields in
	begin match fields with
	| [] ->
		()
	| _ ->
		let jsig = method_sig [string_sig;object_sig] None in
		let jm = jc#spawn_method "_hx_setField" jsig [MPublic;MSynthetic] in
		let _,load1,_ = jm#add_local "name" string_sig VarArgument in
		let _,load2,_ = jm#add_local "value" object_sig VarArgument in
		jm#finalize_arguments;
		load1();
		jm#invokevirtual string_path "hashCode" (method_sig [] (Some TInt));
		let def = (fun () ->
			jm#load_this;
			load1();
			load2();
			jm#invokespecial jc#get_super_path "_hx_setField" jsig;
		) in
		let cases = List.map (fun (name,jsig,_) ->
			let hash = java_hash name in
			[hash],(fun () ->
				jm#load_this;
				load2();
				jm#cast jsig;
				jm#putfield jc#get_this_path name jsig;
				(* Have to deal with Reflect.deleteField crap here... *)
				if is_anon then begin
					jm#load_this;
					jm#getfield jc#get_this_path "_hx_deletedAField" boolean_sig;
					jm#if_then
						(jm#get_code#if_null boolean_sig)
						(fun () ->
							def();
						)
				end;
			)
		) fields in
		jm#int_switch false cases (Some def);
		jm#return
	end

class tclass_to_jvm gctx c = object(self)
	val is_annotation = Meta.has Meta.Annotation c.cl_meta
	val field_inits = DynArray.create ()
	val delayed_field_inits = DynArray.create ()

	val jc = new JvmClass.builder c.cl_path (match c.cl_super with
		| Some(c,_) -> c.cl_path
		| None ->
			if (has_class_flag c CInterface) || Meta.has Meta.NativeGen c.cl_meta then object_path else haxe_object_path
		)

	method private set_access_flags =
		jc#add_access_flag 1; (* public *)
		if has_class_flag c CFinal then jc#add_access_flag 0x10;
		if (has_class_flag c CInterface) then begin
			jc#add_access_flag 0x200;
			jc#add_access_flag 0x400;
		end;
		if is_annotation then begin
			jc#add_access_flag 0x2000;
			jc#add_interface (["java";"lang";"annotation"],"Annotation") [];
			(* TODO: this should be done via Haxe metadata instead of hardcoding it here *)
			jc#add_annotation retention_path ["value",(AEnum(retention_policy_sig,"RUNTIME"))];
		end;
		if (has_class_flag c CAbstract) then jc#add_access_flag 0x0400; (* abstract *)
		if Meta.has Meta.JvmSynthetic c.cl_meta then jc#add_access_flag 0x1000 (* synthetic *)

	method private build_bridges =
		let make_bridge name jsig_from jsig_to =
			let args_from,ret_from = match jsig_from with
				| TMethod(jsigs,jsig) -> jsigs,jsig
				| _ -> die "" __LOC__
			in
			let args_to,ret_to = match jsig_to with
				| TMethod(jsigs,jsig) -> jsigs,jsig
				| _ -> die "" __LOC__
			in
			let jm = jc#spawn_method name jsig_from [MPublic;MSynthetic;MBridge] in
			gctx.typed_functions#make_forward_method_jsig jc jm name args_from ret_from args_to ret_to
		in
		let maybe_make_bridge name jsig_from jsig_to =
			if not (jc#has_method name jsig_from) then make_bridge name jsig_from jsig_to
		in
		let compare_fields cf_impl cf_super =
			let jsig_super = jsignature_of_type gctx cf_super.cf_type in
			let jsig_impl = jsignature_of_type gctx cf_impl.cf_type in
			if jsig_super <> jsig_impl then
				maybe_make_bridge cf_impl.cf_name jsig_super jsig_impl
		in
		let find_overload map_type c cf =
			let tl = match follow (map_type cf.cf_type) with
				| TFun(tl,_) -> tl
				| _ -> die "" __LOC__
			in
			OverloadResolution.resolve_instance_overload false map_type c cf.cf_name (List.map (fun (_,_,t) -> Texpr.Builder.make_null t null_pos) tl)
		in
		let if_method f cf = match cf.cf_kind with
			| Method _ ->
				f cf;
				List.iter f cf.cf_overloads
			| _ ->
				()
		in
		begin match c.cl_super with
		| Some (c_sup,tl) ->
			let map_type = apply_params c_sup.cl_params tl in
			let check_override cf =
				if has_class_field_flag cf CfOverload then begin match find_overload map_type c_sup cf with
				| Some (_,cf_super,_) ->
					compare_fields cf cf_super
				| None ->
					()
				end else begin
					let _,_,cf_super = raw_class_field (fun cf -> cf.cf_type) c_sup (List.map snd c_sup.cl_params) cf.cf_name in
					compare_fields cf cf_super
				end
			in
			let check cf =
				if has_class_field_flag cf CfOverride then check_override cf;
			in
			List.iter (if_method check) c.cl_ordered_fields
		| None ->
			()
		end;
		let rec check_interface map_type (c_int,tl) =
			let map_type t = map_type (apply_params c_int.cl_params tl t) in
			let check cf =
				begin match find_overload map_type c cf with
				| Some (_,cf_impl,_) ->
					compare_fields cf_impl cf
				| None ->
					()
				end
			in
			List.iter (if_method check) c_int.cl_ordered_fields;
			List.iter (check_interface map_type) c_int.cl_implements
		in
		List.iter (check_interface (fun t -> t)) c.cl_implements

	method private set_interfaces =
		List.iter (fun (c_int,tl) ->
			if is_annotation && c_int.cl_path = (["java";"lang";"annotation"],"Annotation") then
				()
			else begin
				jc#add_interface c_int.cl_path (List.map (jtype_argument_of_type gctx []) tl)
			end
		) c.cl_implements

	method private generate_empty_ctor =
		let jsig_empty = method_sig [haxe_empty_constructor_sig] None in
		let jm_empty_ctor = jc#spawn_method "<init>" jsig_empty [MPublic;MSynthetic] in
		let _,load,_ = jm_empty_ctor#add_local "_" haxe_empty_constructor_sig VarArgument in
		jm_empty_ctor#load_this;
		if c.cl_constructor = None then begin
			let handler = new texpr_to_jvm gctx jc jm_empty_ctor None in
			DynArray.iter (fun e ->
				handler#texpr RVoid e;
			) field_inits;
		end;
		begin match c.cl_super with
		| None ->
			(* Haxe type with no parent class, call Object.<init>() *)
			jm_empty_ctor#call_super_ctor ConstructInit (method_sig [] None)
		| _ ->
			(* Parent class exists, call SuperClass.<init>(EmptyConstructor) *)
			load();
			jm_empty_ctor#call_super_ctor ConstructInit jsig_empty
		end;
		if c.cl_constructor = None then begin
			let handler = new texpr_to_jvm gctx jc jm_empty_ctor None in
			DynArray.iter (fun e ->
				handler#texpr RVoid e;
			) delayed_field_inits;
		end;
		jm_empty_ctor#return;

	method private generate_implicit_ctors =
		try
			let sm = gctx.preprocessor#get_implicit_ctor c in
			PMap.iter (fun _ (c,cf) ->
				let cmode = get_construction_mode c cf in
				let jm = jc#spawn_method (if cmode = ConstructInit then "<init>" else "new") (jsignature_of_type gctx cf.cf_type) [MPublic] in
				let handler = new texpr_to_jvm gctx jc jm None in
				jm#load_this;
				DynArray.iter (fun e ->
					handler#texpr RVoid e;
				) field_inits;
				let tl = match follow cf.cf_type with TFun(tl,_) -> tl | _ -> die "" __LOC__ in
				List.iter (fun (n,_,t) ->
					let _,load,_ = jm#add_local n (jsignature_of_type gctx t) VarArgument in
					load();
				) tl;
				jm#call_super_ctor cmode jm#get_jsig;
				DynArray.iter (fun e ->
					handler#texpr RVoid e;
				) delayed_field_inits;
				jm#return
			) sm
		with Not_found ->
			()

	method generate_expr gctx jc jm e is_method scmode mtype =
		let e,args,tr = match e.eexpr with
			| TFunction tf when is_method ->
				tf.tf_expr,tf.tf_args,(return_of_type gctx tf.tf_type)
			| _ ->
				e,[],None
		in
		let handler = new texpr_to_jvm gctx jc jm tr in
		List.iter (fun (v,_) ->
			let slot,_,_ = handler#add_local v VarArgument in
			let annot = AnnotationHandler.convert_annotations v.v_meta in
			match annot with
			| [] ->
				()
			| _ ->
				jm#add_argument_annotation slot annot;
		) args;
		jm#finalize_arguments;
		begin match mtype with
		| MConstructor ->
			DynArray.iter (fun e ->
				handler#texpr RVoid e;
			) field_inits;
			begin match scmode with
			| SCJava ->
				handler#object_constructor
			| SCHaxe ->
				jm#load_this;
				jm#get_code#aconst_null jc#get_jsig;
				jm#call_super_ctor ConstructInit (method_sig [haxe_empty_constructor_sig] None);
			| SCNone ->
				()
			end;
			DynArray.iter (fun e ->
				handler#texpr RVoid e;
			) delayed_field_inits;
		| _ ->
			()
		end;
			handler#texpr RReturn e

	method generate_method gctx jc c mtype cf =
		gctx.current_field_info <- gctx.preprocessor#get_field_info cf.cf_meta;
		let jsig = jsignature_of_type gctx cf.cf_type in
		let flags = if Meta.has Meta.Private cf.cf_meta then [MPrivate] else if Meta.has Meta.Protected cf.cf_meta then [MProtected] else [MPublic] in
		let flags = if (has_class_flag c CInterface) then MAbstract :: flags else flags in
		let flags = if mtype = MStatic then MethodAccessFlags.MStatic :: flags else flags in
		let flags = if has_class_field_flag cf CfFinal then MFinal :: flags else flags in
		let flags = if Meta.has Meta.JvmSynthetic cf.cf_meta then MSynthetic :: flags else flags in
		let flags = if Meta.has Meta.NativeJni cf.cf_meta then MNative :: flags else flags in
		let flags = if (has_class_field_flag cf CfAbstract) then MAbstract :: flags else flags in
		let name,scmode,flags = match mtype with
			| MConstructor ->
				let rec has_super_ctor c = match c.cl_super with
					| None -> false
					| Some(c,_) -> c.cl_constructor <> None || has_super_ctor c
				in
				let get_scmode () = if c.cl_super = None then SCJava else if not (has_super_ctor c) then SCHaxe else SCNone in
				if get_construction_mode c cf = ConstructInit then "<init>",get_scmode(),flags
				else cf.cf_name,SCNone,flags
			| _ -> cf.cf_name,SCNone,flags
		in
		let jm = jc#spawn_method name jsig flags in
		begin match cf.cf_expr with
		| None -> ()
		| Some e ->
			self#generate_expr gctx jc jm e true scmode mtype;
		end;
		begin match cf.cf_params with
			| [] when c.cl_params = [] ->
				()
			| _ ->
				let stl = String.concat "" (List.map (fun (n,_) ->
					Printf.sprintf "%s:Ljava/lang/Object;" n
				) cf.cf_params) in
				let ssig = generate_method_signature true (jsignature_of_type gctx cf.cf_type) in
				let s = if cf.cf_params = [] then ssig else Printf.sprintf "<%s>%s" stl ssig in
				let offset = jc#get_pool#add_string s in
				jm#add_attribute (AttributeSignature offset);
		end;
		AnnotationHandler.generate_annotations (jm :> JvmBuilder.base_builder) cf.cf_meta;

	method generate_field gctx (jc : JvmClass.builder) c mtype cf =
		let jsig = jsignature_of_type gctx cf.cf_type in
		let flags = if Meta.has Meta.Private cf.cf_meta then [FdPrivate] else if Meta.has Meta.Protected cf.cf_meta then [FdProtected] else [FdPublic] in
		let flags = if mtype = MStatic then FdStatic :: flags else flags in
		let flags = if Meta.has Meta.JvmSynthetic cf.cf_meta then FdSynthetic :: flags else flags in
		let jm = jc#spawn_field cf.cf_name jsig flags in
		let default e =
			let p = null_pos in
			let efield = Texpr.Builder.make_static_field c cf p in
			let eop = mk (TBinop(OpAssign,efield,e)) cf.cf_type p in
			begin match c.cl_init with
			| None -> c.cl_init <- Some eop
			| Some e -> c.cl_init <- Some (concat e eop)
			end
		in
		begin match cf.cf_expr with
			| None ->
				if c.cl_path = (["haxe"],"Resource") && cf.cf_name = "content" then begin
					let el = Hashtbl.fold (fun name _ acc ->
						Texpr.Builder.make_string gctx.com.basic name null_pos :: acc
					) gctx.com.resources [] in
					let e = mk (TArrayDecl el) (gctx.com.basic.tarray gctx.com.basic.tstring) null_pos in
					default e;
				end;
			| Some e when mtype <> MStatic ->
				let tl = List.map snd c.cl_params in
				let ethis = mk (TConst TThis) (TInst(c,tl)) null_pos in
				let efield = mk (TField(ethis,FInstance(c,tl,cf))) cf.cf_type null_pos in
				let eop = mk (TBinop(OpAssign,efield,e)) cf.cf_type null_pos in
				DynArray.add (match cf.cf_kind with Method MethDynamic -> delayed_field_inits | _ -> field_inits) eop;
			| Some e ->
				match e.eexpr with
				| TConst ct ->
					begin match ct with
					| TInt i32 when not (is_nullable cf.cf_type) ->
						let offset = jc#get_pool#add (ConstInt i32) in
						jm#add_attribute (AttributeConstantValue offset);
					| TString s ->
						let offset = jc#get_pool#add_const_string s in
						jm#add_attribute (AttributeConstantValue offset);
					| _ ->
						default e;
					end
				| _ ->
					default e;
		end;
		let ssig = generate_signature true (jsignature_of_type gctx cf.cf_type) in
		let offset = jc#get_pool#add_string ssig in
		jm#add_attribute (AttributeSignature offset);
		AnnotationHandler.generate_annotations (jm :> JvmBuilder.base_builder) cf.cf_meta;

	method generate_main e =
		let jsig = method_sig [array_sig string_sig] None in
		let jm = jc#spawn_method "main" jsig [MPublic;MStatic] in
		let _,load,_ = jm#add_local "args" (TArray(string_sig,None)) VarArgument in
		if has_feature gctx.com "haxe.root.Sys.args" then begin
			load();
			jm#putstatic (["haxe";"root"],"Sys") "_args" (TArray(string_sig,None))
		end;
		jm#invokestatic (["haxe"; "java"], "Init") "init" (method_sig [] None);
		self#generate_expr gctx jc jm e true SCNone MStatic;
		if not jm#is_terminated then jm#return

	method private generate_fields =
		let field mtype cf = match cf.cf_kind with
			| Method (MethNormal | MethInline) ->
				List.iter (fun cf ->
					if not (has_class_field_flag cf CfExtern) then self#generate_method gctx jc c mtype cf
				) (cf :: List.filter (fun cf -> has_class_field_flag cf CfOverload) cf.cf_overloads)
			| _ ->
				if not (has_class_flag c CInterface) && is_physical_field cf then self#generate_field gctx jc c mtype cf
		in
		let field mtype cf =
			run_timed gctx true cf.cf_name (fun () -> field mtype cf)
		in
		Option.may (fun (c2,e) -> if c2 == c then self#generate_main e) gctx.entry_point;
		List.iter (field MStatic) c.cl_ordered_statics;
		List.iter (field MInstance) c.cl_ordered_fields;
		begin match c.cl_constructor,c.cl_super with
			| Some cf,Some _ -> field MConstructor cf
			| Some cf,None -> field MConstructor cf
			| None,_ -> ()
		end;
		begin match c.cl_init with
			| None ->
				()
			| Some e ->
				let jm = jc#get_static_init_method in
				let handler = new texpr_to_jvm gctx jc jm None in
				handler#texpr RReturn (mk_block e);
		end

	method private generate_signature =
		jc#set_type_parameters (List.map (fun (n,t) ->
			let jsigs = match follow t with
			| TInst({cl_kind = KTypeParameter tl},_) ->
				List.map (fun t ->
					get_boxed_type (jsignature_of_type gctx t)
				 ) tl
			| _ ->
				[]
			in
			(n,jsigs)
		) c.cl_params);
		match c.cl_super with
			| Some(c,tl) -> jc#set_super_parameters (List.map (jtype_argument_of_type gctx []) tl)
			| _ -> ()

	method generate_annotations =
		AnnotationHandler.generate_annotations (jc :> JvmBuilder.base_builder) c.cl_meta;
		jc#add_annotation (["haxe";"jvm";"annotation"],"ClassReflectionInformation") (["hasSuperClass",(ABool (c.cl_super <> None))])

	method private do_generate =
		self#set_access_flags;
		jc#set_source_file c.cl_pos.pfile;
		run_timed gctx true "fields" (fun () -> self#generate_fields);
		self#set_interfaces;
		if not (has_class_flag c CInterface) then begin
			self#generate_empty_ctor;
			self#generate_implicit_ctors;
			self#build_bridges;
		end;
		self#generate_signature;
		if gctx.dynamic_level > 0 && not (Meta.has Meta.NativeGen c.cl_meta) && not (has_class_flag c CInterface) then
			generate_dynamic_access gctx jc (List.map (fun cf -> cf.cf_name,jsignature_of_type gctx cf.cf_type,cf.cf_kind) c.cl_ordered_fields) false;
		self#generate_annotations;
		let jc = jc#export_class gctx.default_export_config in
		write_class gctx c.cl_path jc

	method generate =
		run_timed gctx true (s_type_path c.cl_path) (fun () -> self#do_generate)
end

let generate_class gctx c =
	let conv = new tclass_to_jvm gctx c in
	conv#generate

let generate_enum_equals gctx (jc_ctor : JvmClass.builder) =
	let jm_equals,load = generate_equals_function jc_ctor (haxe_enum_sig object_sig) in
	let code = jm_equals#get_code in
	let jm_equals_handler = new texpr_to_jvm gctx jc_ctor jm_equals (Some TBool) in
	let is_haxe_enum jsig = match jsig with
		| TObject(path,_) ->
			Hashtbl.mem gctx.enum_paths path
		| _ ->
			false
	in
	let compare_whatever jsig =
		jm_equals#invokestatic haxe_jvm_path "maybeEnumEq" (method_sig [object_sig;object_sig] (Some TBool));
		jm_equals#if_then
			(code#if_ CmpNe)
			(fun () ->
				code#bconst false;
				jm_equals#return;
			)
	in
	let compare_haxe_enum jsig =
		jm_equals#invokestatic haxe_type_path "enumEq" (method_sig [object_sig;object_sig] (Some TBool));
		jm_equals#if_then
			(code#if_ CmpNe)
			(fun () ->
				code#bconst false;
				jm_equals#return;
			)
	in
	let compare_standard jsig =
		jm_equals#if_then
			(jm_equals_handler#apply_cmp (jm_equals_handler#do_compare CmpNe))
			(fun () ->
				code#bconst false;
				jm_equals#return;
			);
	in
	let compare jsig =
		if NativeSignatures.is_dynamic_at_runtime jsig then
			compare_whatever jsig
		else if is_haxe_enum jsig then
			compare_haxe_enum jsig
		else
			compare_standard jsig
	in
	load();
	jm_equals#invokevirtual java_enum_path "ordinal" (method_sig [] (Some TInt));
	jm_equals#load_this;
	jm_equals#invokevirtual java_enum_path "ordinal" (method_sig [] (Some TInt));
	compare TInt;
	let compare_field n jsig =
		load();
		jm_equals#getfield jc_ctor#get_this_path n jsig;
		jm_equals#load_this;
		jm_equals#getfield jc_ctor#get_this_path n jsig;
		compare jsig;
	in
	jm_equals,compare_field

let generate_enum gctx en =
	let jc_enum = new JvmClass.builder en.e_path haxe_enum_path in
	jc_enum#add_access_flag 0x1; (* public *)
	jc_enum#add_access_flag 0x400; (* abstract *)
	if Meta.has Meta.JvmSynthetic en.e_meta then jc_enum#add_access_flag 0x1000; (* synthetic *)
	let jsig_enum_ctor = method_sig [TInt;string_sig] None in
	(* Create base constructor *)
	 begin
		let jm_ctor = jc_enum#spawn_method "<init>" jsig_enum_ctor [MProtected] in
		jm_ctor#load_this;
		let _,load1,_ = jm_ctor#add_local "index" TInt VarArgument in
		let _,load2,_ = jm_ctor#add_local "name" string_sig VarArgument in
		load1();
		load2();
		jm_ctor#call_super_ctor ConstructInit jsig_enum_ctor;
		jm_ctor#return;
	end;
	let inits = DynArray.create () in
	let names = List.map (fun name ->
		let ef = PMap.find name en.e_constrs in
		let args = match follow ef.ef_type with
			| TFun(tl,_) -> List.map (fun (n,_,t) -> n,jsignature_of_type gctx t) tl
			| _ -> []
		in
		let jsigs = List.map snd args in
		(* Create class for constructor *)
		let jc_ctor = begin
			let jc_ctor = jc_enum#spawn_inner_class None jc_enum#get_this_path (Some ef.ef_name) in
			jc_ctor#add_access_flag 0x10; (* final *)
			let jsig_method = method_sig jsigs None in
			let jm_ctor = jc_ctor#spawn_method "<init>" jsig_method [MPublic] in
			jm_ctor#load_this;
			jm_ctor#get_code#iconst (Int32.of_int ef.ef_index);
			jm_ctor#string ef.ef_name;
			jm_ctor#call_super_ctor ConstructInit jsig_enum_ctor;
			List.iter (fun (n,jsig) ->
				jm_ctor#add_argument_and_field n jsig [FdPublic;FdFinal]
			) args;
			jm_ctor#return;
			jc_ctor#add_annotation (["haxe";"jvm";"annotation"],"EnumValueReflectionInformation") (["argumentNames",AArray (List.map (fun (name,_) -> AString name) args)]);
			if args <> [] then begin
				let jm_params = jc_ctor#spawn_method "_hx_getParameters" (method_sig [] (Some (array_sig object_sig))) [MPublic;MSynthetic] in
				let jm_equals,compare_field = generate_enum_equals gctx jc_ctor in
				let fl = List.map (fun (n,jsig) ->
					compare_field n jsig;
					(fun () ->
						jm_params#load_this;
						jm_params#getfield jc_ctor#get_this_path n jsig;
						jm_params#cast object_sig;
					)
				) args in
				jm_equals#get_code#bconst true;
				jm_equals#return;
				jm_params#new_native_array object_sig fl;
				jm_params#return
			end;
			jc_ctor
		end in
		write_class gctx jc_ctor#get_this_path (jc_ctor#export_class gctx.default_export_config);
		begin match args with
			| [] ->
				(* Create static field for ctor without args *)
				let jm_static = jc_enum#spawn_field ef.ef_name jc_enum#get_jsig [FdPublic;FdStatic;FdFinal;FdEnum] in
				DynArray.add inits (jm_static,jc_ctor);
			| _ ->
				(* Create static function for ctor with args *)
				let jsig_static = method_sig jsigs (Some jc_enum#get_jsig) in
				let jm_static = jc_enum#spawn_method ef.ef_name jsig_static [MPublic;MStatic] in
				jm_static#construct ConstructInit jc_ctor#get_this_path (fun () ->
					List.iter (fun (n,jsig) ->
						let _,load,_ = jm_static#add_local n jsig VarArgument in
						load();
					) args;
					jsigs;
				);
				jm_static#return;
		end;
		AString name
	) en.e_names in
	if DynArray.length inits > 0 then begin
		(* Assign static fields for ctors without args *)
		let jm_clinit = jc_enum#spawn_method "<clinit>" (method_sig [] None) [MStatic] in
		let jm_values = jc_enum#spawn_method "values" (method_sig [] (Some (array_sig (object_path_sig jc_enum#get_this_path)))) [MPublic;MStatic] in
		let inits = DynArray.to_list inits in
		let fl = List.map (fun (jm_static,jc_ctor) ->
			jm_clinit#construct ConstructInit jc_ctor#get_this_path (fun () -> []);
			jm_clinit#putstatic jc_enum#get_this_path jm_static#get_name jm_static#get_jsig;
			(fun () ->
				jm_values#getstatic jc_enum#get_this_path jm_static#get_name jm_static#get_jsig;
			)
		) inits in
		jm_values#new_native_array (object_path_sig jc_enum#get_this_path) fl;
		jm_values#return;
		(* Add __meta__ TODO: do this via annotations instead? *)
		begin match Texpr.build_metadata gctx.com.basic (TEnumDecl en) with
		| None ->
			()
		| Some e ->
			ignore(jc_enum#spawn_field "__meta__" object_sig [FdStatic;FdPublic]);
			let handler = new texpr_to_jvm gctx jc_enum jm_clinit None in
			handler#texpr rvalue_any e;
			jm_clinit#putstatic jc_enum#get_this_path "__meta__" object_sig
		end;
		jm_clinit#return;
	end;
	AnnotationHandler.generate_annotations (jc_enum :> JvmBuilder.base_builder) en.e_meta;
	jc_enum#add_annotation (["haxe";"jvm";"annotation"],"EnumReflectionInformation") (["constructorNames",AArray names]);
	write_class gctx en.e_path (jc_enum#export_class gctx.default_export_config)

let generate_module_type ctx mt =
	match mt with
		| TClassDecl c when not (has_class_flag c CExtern) -> generate_class ctx c
		| TEnumDecl en when not en.e_extern -> generate_enum ctx en
		| _ -> ()

let generate_anons gctx =
	Hashtbl.iter (fun _ pfm ->
		let path = pfm.pfm_path in
		let fields = convert_fields gctx pfm in
		let jc = new JvmClass.builder path haxe_dynamic_object_path in
		jc#add_access_flag 0x1;
		begin
			let jm_ctor = jc#spawn_method "<init>" (method_sig (List.map snd fields) None) [MPublic] in
			jm_ctor#load_this;
			jm_ctor#get_code#aconst_null haxe_empty_constructor_sig;
			jm_ctor#call_super_ctor ConstructInit (method_sig [haxe_empty_constructor_sig] None);
			List.iter (fun (name,jsig) ->
				jm_ctor#add_argument_and_field name jsig [FdPublic]
			) fields;
			jm_ctor#return;
		end;
		begin
			let string_map_path = (["haxe";"ds"],"StringMap") in
			let string_map_sig = object_path_sig string_map_path in
			let jm_fields = jc#spawn_method "_hx_getKnownFields" (method_sig [] (Some string_map_sig)) [MProtected;MSynthetic] in
			let _,load,save = jm_fields#add_local "tmp" string_map_sig VarWillInit in
			jm_fields#construct ConstructInit string_map_path (fun () -> []);
			save();
			List.iter (fun (name,jsig) ->
				load();
				let offset = jc#get_pool#add_const_string name in
				jm_fields#get_code#sconst (string_sig) offset;
				jm_fields#load_this;
				jm_fields#getfield jc#get_this_path name jsig;
				jm_fields#expect_reference_type;
				jm_fields#invokevirtual string_map_path "set" (method_sig [string_sig;object_sig] None);
			) fields;
			load();
			jm_fields#return
		end;
		(* This has to run even with dynamic_level = 0 because the entire DynamicObject logic depends on it. *)
		generate_dynamic_access gctx jc (List.map (fun (name,jsig) -> name,jsig,Var {v_write = AccNormal;v_read = AccNormal}) fields) true;
		begin match gctx.typedef_interfaces#get_interface_class path with
		| None ->
			()
		| Some c ->
			jc#add_interface c.cl_path [];
			List.iter (fun cf ->
				let jsig_cf = jsignature_of_type gctx cf.cf_type in
				let jm = jc#spawn_method cf.cf_name jsig_cf [MPublic] in
				let tl,tr = match follow cf.cf_type with
					| TFun(tl,tr) -> tl,tr
					| _ -> die "" __LOC__
				in
				let locals = List.map (fun (n,_,t) ->
					let jsig = jsignature_of_type gctx t in
					jm#add_local n jsig VarArgument,jsig
				) tl in
				jm#finalize_arguments;
				jm#load_this;
				jm#getfield path cf.cf_name jsig_cf;
				List.iter (fun ((_,load,_),_) ->
					load();
				) locals;
				let jret = return_of_type gctx tr in
				let meth = gctx.typed_functions#register_signature (List.map snd locals) jret in
				jm#invokevirtual haxe_function_path meth.name (method_sig meth.dargs meth.dret);
				Option.may jm#cast jret;
				jm#return
			) c.cl_ordered_fields
		end;
		write_class gctx path (jc#export_class gctx.default_export_config)
	) gctx.anon_identification#get_pfms

let generate_typed_functions gctx =
	let jc_function = gctx.typed_functions#generate in
	write_class gctx jc_function#get_this_path (jc_function#export_class gctx.default_export_config);
	let jc_varargs = gctx.typed_functions#generate_var_args in
	write_class gctx jc_varargs#get_this_path (jc_varargs#export_class gctx.default_export_config);
	let jc_closure_dispatch = gctx.typed_functions#generate_closure_dispatch in
	write_class gctx jc_closure_dispatch#get_this_path (jc_closure_dispatch#export_class gctx.default_export_config)

module Preprocessor = struct
	let make_root path =
		["haxe";"root"],snd path

	let has_primary_type m =
		List.exists (fun mt -> snd (t_infos mt).mt_path = snd m.m_path) m.m_types

	let check_path mt =
		if mt.mt_private && has_primary_type mt.mt_module && not (Meta.has Meta.Native mt.mt_meta) then begin
			let m = mt.mt_module in
			let pack = match fst m.m_path with
				| [] -> ["haxe";"root"]
				| pack -> pack
			in
			mt.mt_path <- (pack,Printf.sprintf "%s$%s" (snd m.m_path) (snd mt.mt_path))
		end else if fst mt.mt_path = [] then
			mt.mt_path <- make_root mt.mt_path

	let preprocess gctx =
		let rec has_runtime_meta = function
			| (Meta.Custom s,_,_) :: _ when String.length s > 0 && s.[0] <> ':' ->
				true
			| _ :: l ->
				has_runtime_meta l
			| [] ->
				false
		in
		(* go through com.modules so we can also pick up private typedefs *)
		List.iter (fun m ->
			List.iter (fun mt ->
				match mt with
				| TClassDecl c when has_runtime_meta c.cl_meta && has_class_flag c CInterface ->
					() (* TODO: run-time interface metadata is a problem (issue #2042) *)
				| TClassDecl _ | TEnumDecl _ ->
					check_path (t_infos mt);
				| TTypeDecl td ->
					check_path (t_infos mt);
					gctx.anon_identification#identify_typedef td
				| _ ->
					()
			) m.m_types
		) gctx.com.modules;
		(* preprocess classes *)
		List.iter (fun mt ->
			match mt with
			| TClassDecl c ->
				if not (has_class_flag c CInterface) then gctx.preprocessor#preprocess_class c
			| _ -> ()
		) gctx.com.types;
		(* find typedef-interface implementations *)
		List.iter (fun mt -> match mt with
			| TClassDecl c when not (has_class_flag c CInterface) && not (has_class_flag c CExtern) ->
				gctx.typedef_interfaces#process_class c;
			| _ ->
				()
		) gctx.com.types
end

let generate jvm_flag com =
	let path = FilePath.parse com.file in
	let jar_name,entry_point = match get_entry_point com with
		| Some (jarname,cl,expr) -> jarname, Some (cl,expr)
		| None -> "jar",None
	in
	let jar_dir,jar_path = if jvm_flag then begin
		match path.file_name with
		| Some _ ->
			begin match path.directory with
				| None ->
					"./","./" ^ com.file
				| Some dir ->
					mkdir_from_path dir;
					add_trailing_slash dir,com.file
			end
		| None ->
			failwith "Please specify an output file name"
	end else begin
		let jar_name = if com.debug then jar_name ^ "-Debug" else jar_name in
		let jar_dir = add_trailing_slash com.file in
		let jar_path = Printf.sprintf "%s%s.jar" jar_dir jar_name in
		jar_dir,jar_path
	end in
	let anon_identification = new tanon_identification haxe_dynamic_object_path in
	let compression_level = try
		int_of_string (Define.defined_value com.defines Define.JvmCompressionLevel)
	with _ ->
		6
	in
	if compression_level < 0 || compression_level > 9 then failwith "Invalid value for -D jvm.compression-level: Must be >=0 and <= 9";
	let dynamic_level = try
		int_of_string (Define.defined_value com.defines Define.JvmDynamicLevel)
	with _ ->
		1
	in
	if dynamic_level < 0 || dynamic_level > 2 then failwith "Invalid value for -D jvm.dynamic-level: Must be >=0 and <= 2";
	let gctx = {
		com = com;
		jar = Zip.open_out jar_path;
		t_runtime_exception = TInst(resolve_class com (["java";"lang"],"RuntimeException"),[]);
		entry_point = entry_point;
		t_exception = TInst(resolve_class com (["java";"lang"],"Exception"),[]);
		t_throwable = TInst(resolve_class com (["java";"lang"],"Throwable"),[]);
		anon_identification = anon_identification;
		preprocessor = Obj.magic ();
		typedef_interfaces = Obj.magic ();
		typed_functions = new JvmFunctions.typed_functions;
		closure_paths = Hashtbl.create 0;
		enum_paths = Hashtbl.create 0;
		current_field_info = None;
		default_export_config = {
			export_debug = true;
		};
		detail_times = Common.raw_defined com "jvm_times";
		timer = new Timer.timer ["generate";"java"];
		jar_compression_level = compression_level;
		dynamic_level = dynamic_level;
	} in
	gctx.anon_identification <- anon_identification;
	gctx.preprocessor <- new preprocessor com.basic (jsignature_of_type gctx);
	gctx.typedef_interfaces <- new typedef_interfaces gctx.preprocessor#get_infos anon_identification;
	gctx.typedef_interfaces#add_interface_rewrite (["haxe";"root"],"Iterator") (["java";"util"],"Iterator") true;
	let class_paths = ExtList.List.filter_map (fun java_lib ->
		if java_lib#has_flag NativeLibraries.FlagIsStd || java_lib#has_flag FlagIsExtern then None
		else begin
			let dir = Printf.sprintf "%slib/" jar_dir in
			Path.mkdir_from_path dir;
			let name = FilePath.name_and_extension (FilePath.parse java_lib#get_file_path) in
			let ch_in = open_in_bin java_lib#get_file_path in
			let ch_out = open_out_bin (Printf.sprintf "%s%s" dir name) in
			let b = IO.read_all (IO.input_channel ch_in) in
			output_string ch_out b;
			close_in ch_in;
			close_out ch_out;
			Some (Printf.sprintf "lib/%s \n" name)
		end
	) com.native_libs.java_libs in
	Hashtbl.iter (fun name v ->
		let filename = Codegen.escape_res_name name true in
		Zip.add_entry ~level:gctx.jar_compression_level v gctx.jar filename;
	) com.resources;
	let generate_real_types () =
		List.iter (generate_module_type gctx) com.types;
	in
	let generate_typed_interfaces () =
		Hashtbl.iter (fun _ c -> generate_module_type gctx (TClassDecl c)) gctx.typedef_interfaces#get_interfaces;
	in
	run_timed gctx false "preprocess" (fun () -> Preprocessor.preprocess gctx);
	run_timed gctx false "real types" generate_real_types;
	run_timed gctx false "typed interfaces" generate_typed_interfaces;
	run_timed gctx false "anons" (fun () -> generate_anons gctx);
	run_timed gctx false "typed_functions" (fun () -> generate_typed_functions gctx);

	let manifest_content =
		"Manifest-Version: 1.0\n" ^
		"Created-By: Haxe (Haxe Foundation)" ^
		(Option.map_default (fun (cl,_) ->  "\nMain-Class: " ^ (s_type_path cl.cl_path)) "" entry_point) ^
		(match class_paths with [] -> "" | _ -> "\nClass-Path: " ^ (String.concat " " class_paths)) ^
		"\n\n"
	in
	Zip.add_entry ~level:gctx.jar_compression_level manifest_content gctx.jar "META-INF/MANIFEST.MF";

	Zip.close_out gctx.jar
