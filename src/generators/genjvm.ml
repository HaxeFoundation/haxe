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

(* Note: This module is the bridge between Haxe structures and JVM structures. No module in generators/jvm should reference any
   Haxe-specific type. *)

(* hacks *)

let rec pow a b = match b with
	| 0 -> Int32.one
	| 1 -> a
	| _ -> Int32.mul a (pow a (b - 1))

let java_hash s =
	let h = ref Int32.zero in
	let l = UTF8.length s in
	let i31 = Int32.of_int 31 in
	let i = ref 0 in
	UTF8.iter (fun char ->
		let char = Int32.of_int (UCharExt.uint_code char) in
		h := Int32.add !h (Int32.mul char (pow i31 (l - (!i + 1))));
		incr i;
	) s;
	!h

let find_overload map_type c cf el =
	let matches = ref [] in
	let rec loop cfl = match cfl with
		| cf :: cfl ->
			begin match follow (monomorphs cf.cf_params (map_type cf.cf_type)) with
				| TFun(tl'',_) as tf ->
					let rec loop2 acc el tl = match el,tl with
						| e :: el,(n,o,t) :: tl ->
							begin try
								Type.unify e.etype t;
								loop2 ((e,o) :: acc) el tl
							with _ ->
								loop cfl
							end
						| [],[] ->
							matches := ((List.rev acc),tf,(c,cf)) :: !matches;
							loop cfl
						| _ ->
							loop cfl
					in
					loop2 [] el tl''
				| t ->
					loop cfl
			end;
		| [] ->
			List.rev !matches
	in
	loop (cf :: cf.cf_overloads)

let find_overload_rec' is_ctor map_type c name el =
	let candidates = ref [] in
	let has_function t1 (_,t2,_) =
		begin match follow t1,t2 with
		| TFun(tl1,_),TFun(tl2,_) -> type_iseq (TFun(tl1,t_dynamic)) (TFun(tl2,t_dynamic))
		| _ -> false
		end
	in
	let rec loop map_type c =
		begin try
			let cf = if is_ctor then
				(match c.cl_constructor with Some cf -> cf | None -> raise Not_found)
			else
				PMap.find name c.cl_fields
			in
			begin match find_overload map_type c cf el with
			| [] -> raise Not_found
			| l ->
				List.iter (fun ((_,t,_) as ca) ->
					if not (List.exists (has_function t) !candidates) then candidates := ca :: !candidates
				) l
			end;
			if Meta.has Meta.Overload cf.cf_meta || cf.cf_overloads <> [] then raise Not_found
		with Not_found ->
			if c.cl_interface then
				List.iter (fun (c,tl) -> loop (fun t -> apply_params c.cl_params (List.map map_type tl) t) c) c.cl_implements
			else match c.cl_super with
			| None -> ()
			| Some(c,tl) -> loop (fun t -> apply_params c.cl_params (List.map map_type tl) t) c
		end;
	in
	loop map_type c;
	match Overloads.Resolution.reduce_compatible (List.rev !candidates) with
	| [_,_,(c,cf)] -> Some(c,cf)
	| [] -> None
	| ((_,_,(c,cf)) :: _) (* as resolved *) ->
		(* let st = s_type (print_context()) in
		print_endline (Printf.sprintf "Ambiguous overload for %s(%s)" name (String.concat ", " (List.map (fun e -> st e.etype) el)));
		List.iter (fun (_,t,(c,cf)) ->
			print_endline (Printf.sprintf "\tCandidate: %s.%s(%s)" (s_type_path c.cl_path) cf.cf_name (st t));
		) resolved; *)
		Some(c,cf)

let find_overload_rec is_ctor map_type c cf el =
	if Meta.has Meta.Overload cf.cf_meta || cf.cf_overloads <> [] then
		find_overload_rec' is_ctor map_type c cf.cf_name el
	else
		Some(c,cf)

let get_construction_mode c cf =
	if Meta.has Meta.HxGen cf.cf_meta then ConstructInitPlusNew
	else ConstructInit

(* Haxe *)

exception HarderFailure of string

type field_generation_info = {
	mutable has_this_before_super : bool;
	(* This is an ordered list of fields that are targets of super() calls which is determined during
	   pre-processing. The generator can pop from this list assuming that it processes the expression
	   in the same order (which it should). *)
	mutable super_call_fields : (tclass * tclass_field) list;
}

type generation_context = {
	com : Common.context;
	jar : Zip.out_file;
	t_exception : Type.t;
	t_throwable : Type.t;
	anon_lut : ((string * jsignature) list,jpath) Hashtbl.t;
	anon_path_lut : (path,jpath) Hashtbl.t;
	field_infos : field_generation_info DynArray.t;
	implicit_ctors : (path,(path * jsignature,tclass * tclass_field) PMap.t) Hashtbl.t;
	default_export_config : export_config;
	mutable current_field_info : field_generation_info option;
	mutable anon_num : int;
}

type ret =
	| RValue of jsignature option
	| RVoid
	| RReturn

type method_type =
	| MStatic
	| MInstance
	| MConstructor

type access_kind =
	| AKPost
	| AKPre
	| AKNone

type compare_kind =
	| CmpNormal of jcmp * jsignature
	| CmpSpecial of (unit -> jbranchoffset ref)

type block_exit =
	| ExitExecute of (unit -> unit)
	| ExitLoop

open NativeSignatures

let rec jsignature_of_type stack t =
	if List.exists (fast_eq t) stack then object_sig else
	let jsignature_of_type = jsignature_of_type (t :: stack) in
	let jtype_argument_of_type t = jtype_argument_of_type stack t in
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
			| [],"Null" ->
				begin match tl with
				| [t] -> get_boxed_type (jsignature_of_type t)
				| _ -> assert false
				end
			| (["haxe";"ds"],"Vector") | (["haxe";"extern"],"Rest") ->
				begin match tl with
				| [t] -> TArray(jsignature_of_type t,None)
				| _ -> assert false
				end
			| [],"Dynamic" ->
				object_sig
			| [],("Class" | "Enum") ->
				java_class_sig
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
		begin match !r with
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
	| TEnum(en,tl) -> TObject(en.e_path,List.map jtype_argument_of_type tl)
	| TFun(tl,tr) -> method_sig (List.map (fun (_,o,t) ->
		let jsig = jsignature_of_type t in
		let jsig = if o then get_boxed_type jsig else jsig in
		jsig
	) tl) (if ExtType.is_void (follow tr) then None else Some (jsignature_of_type tr))
	| TAnon an -> object_sig
	| TType(td,tl) -> jsignature_of_type (apply_params td.t_params tl td.t_type)
	| TLazy f -> jsignature_of_type (lazy_type f)

and jtype_argument_of_type stack t =
	TType(WNone,jsignature_of_type stack t)

let jsignature_of_type t =
	jsignature_of_type [] t

module TAnonIdentifiaction = struct
	let convert_fields fields =
		let l = PMap.fold (fun cf acc -> cf :: acc) fields [] in
		let l = List.sort (fun cf1 cf2 -> compare cf1.cf_name cf2.cf_name) l in
		List.map (fun cf -> cf.cf_name,jsignature_of_type cf.cf_type) l

	let identify gctx fields =
		if PMap.is_empty fields then
			haxe_dynamic_object_path,[]
		else begin
			let l = convert_fields fields in
			try
				Hashtbl.find gctx.anon_lut l,l
			with Not_found ->
				let id = gctx.anon_num in
				gctx.anon_num <- gctx.anon_num + 1;
				let path = (["haxe";"generated"],Printf.sprintf "Anon%i" id) in
				Hashtbl.add gctx.anon_lut l path;
				path,l
		end

	let identify_as gctx path fields =
		if not (PMap.is_empty fields) && not (Hashtbl.mem gctx.anon_path_lut path) then begin
			let fields = convert_fields fields in
			Hashtbl.add gctx.anon_lut fields path;
			Hashtbl.add gctx.anon_path_lut path path;
		end

end

module AnnotationHandler = struct
	let generate_annotations builder meta =
		let parse_path e =
			let sl = try string_list_of_expr_path_raise e with Exit -> Error.error "Field expression expected" (pos e) in
			let path = match sl with
				| s :: sl -> List.rev sl,s
				| _ -> Error.error "Field expression expected" (pos e)
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
			| _ -> Error.error "Expected value expression" (pos e)
		in
		let parse_value_pair e = match fst e with
			| EBinop(OpAssign,(EConst(Ident s),_),e1) ->
				s,parse_value e1
			| _ ->
				Error.error "Assignment expression expected" (pos e)
		in
		let parse_expr e = match fst e with
			| ECall(e1,el) ->
				let path = parse_path e1 in
				let _,name = ExtString.String.replace (snd path) "." "$" in
				let path = (fst path,name) in
				let values = List.map parse_value_pair el in
				path,values
			| _ ->
				Error.error "Call expression expected" (pos e)
		in
		List.iter (fun (m,el,_) -> match m,el with
			| Meta.Meta,[e] ->
				let path,annotation = parse_expr e in
				builder#add_annotation path annotation;
			| _ ->
				()
		) meta
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
	| _ -> assert false

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

let write_class jar path jc =
	let dir = match path with
		| ([],s) -> s
		| (sl,s) -> String.concat "/" sl ^ "/" ^ s
	in
	let path = dir ^ ".class" in
	let t = Timer.timer ["jvm";"write"] in
	let ch = IO.output_bytes() in
	JvmWriter.write_jvm_class ch jc;
	Zip.add_entry (Bytes.unsafe_to_string (IO.close_out ch)) jar path;
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
	c.cl_interface && match cf.cf_kind with
		| Var _ | Method MethDynamic -> true
		| _ -> false

let type_unifies a b =
	try Type.unify a b; true with _ -> false

let get_field_info gctx ml =
	let rec loop ml = match ml with
	| (Meta.Custom ":jvm.fieldInfo",[(EConst (Int s),_)],_) :: _ ->
		Some (DynArray.get gctx.field_infos (int_of_string s))
	| _ :: ml ->
		loop ml
	| [] ->
		None
	in
	loop ml

let follow = Abstract.follow_with_abstracts

class haxe_exception gctx (t : Type.t) = object(self)
	val native_exception =
		if follow t == t_dynamic then
			throwable_sig,false
		else if type_unifies t gctx.t_exception then
			jsignature_of_type t,true
		else
			haxe_exception_sig,false

	val mutable native_exception_path = None

	method is_assignable_to (exc2 : haxe_exception) =
		match self#is_native_exception,exc2#is_native_exception with
		| true, true ->
			(* Native exceptions are assignable if they unify *)
			type_unifies t exc2#get_type
		| false,false ->
			(* Haxe exceptions are always assignable to each other *)
			true
		| false,true ->
			(* Haxe exception is assignable to native only if caught type is java.lang.Exception/Throwable *)
			let exc2_native_exception_type = exc2#get_native_exception_type in
			exc2_native_exception_type = throwable_sig || exc2_native_exception_type = exception_sig
		| _ ->
			(* Native to Haxe is never assignable *)
			false

	method is_native_exception = snd native_exception
	method get_native_exception_type = fst native_exception

	method get_native_exception_path =
		match native_exception_path with
		| None ->
			let path = (match (fst native_exception) with TObject(path,_) -> path | _ -> assert false) in
			native_exception_path <- Some path;
			path
		| Some path ->
			path

	method get_type = t
end

class closure_context (jsig : jsignature) = object(self)
	val lut = Hashtbl.create 0
	val sigs = DynArray.create()

	method add (var_id : int) (var_name : string) (var_sig : jsignature) =
		DynArray.add sigs ((var_id,var_name),var_sig);
		Hashtbl.add lut var_id (var_sig,var_name)

	method get (code : JvmCode.builder) (var_id : int) =
		let var_sig,var_name = Hashtbl.find lut var_id in
		if DynArray.length sigs > 1 then begin
			(-1),
			(fun () ->
				code#aload jsig 0;
				let offset = code#get_pool#add_field self#get_path var_name var_sig FKField in
				code#getfield offset jsig var_sig
			),
			(fun () ->
				code#aload jsig 0;
				let offset = code#get_pool#add_field self#get_path var_name var_sig FKField in
				code#putfield offset jsig var_sig
			)
		end else begin
			(-1),
			(fun () ->
				code#aload jsig 0;
			),
			(fun () ->
				code#aload jsig 0;
			)
		end

	method get_constructor_sig =
		method_sig (List.map snd (DynArray.to_list sigs)) None

	method get_jsig = jsig
	method get_path = match jsig with TObject(path,_) -> path | _ -> assert false

	method get_args = DynArray.to_list sigs
end

let create_context_class gctx jc jm name vl = match vl with
	| [(vid,vname,vsig)] ->
		let jsig = get_boxed_type vsig in
		let ctx_class = new closure_context jsig in
		ctx_class#add vid vname jsig;
		ctx_class
	| _ ->
		let jc = jc#spawn_inner_class (Some jm) object_path None in
		let path = jc#get_this_path in
		let ctx_class = new closure_context (object_path_sig path) in
		let jsigs = List.map (fun (_,_,vsig) -> vsig) vl in
		let jm_ctor = jc#spawn_method "<init>" (method_sig jsigs None) [MPublic] in
		jm_ctor#load_this;
		jm_ctor#call_super_ctor ConstructInit (method_sig [] None);
		List.iter2 (fun (vid,vname,vtype) jsig ->
			jm_ctor#add_argument_and_field vname jsig;
			ctx_class#add vid vname jsig;
		) vl jsigs;
		jm_ctor#get_code#return_void;
		write_class gctx.jar path (jc#export_class gctx.default_export_config);
		ctx_class

let rvalue_any = RValue None
let rvalue_sig jsig = RValue (Some jsig)
let rvalue_type t = RValue (Some (jsignature_of_type t))

class texpr_to_jvm gctx (jc : JvmClass.builder) (jm : JvmMethod.builder) (return_type : Type.t) = object(self)
	val com = gctx.com
	val code = jm#get_code
	val pool : JvmConstantPool.constant_pool = jc#get_pool

	val mutable local_lookup = Hashtbl.create 0;
	val mutable last_line = 0

	val mutable breaks = []
	val mutable continue = 0
	val mutable caught_exceptions = []
	val mutable block_exits = []
	val mutable env = None

	method vtype t =
		jsignature_of_type t

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
		if vid = 0 then
			(0,(fun () -> jm#load_this),(fun () -> assert false))
		else try
			Hashtbl.find local_lookup vid
		with Not_found -> try
			begin match env with
			| Some env ->
				env#get code vid
			| None ->
				raise Not_found
			end
		with Not_found ->
			failwith ("Unbound local: " ^ vname)

	method get_local v =
		self#get_local_by_id (v.v_id,v.v_name)

	method set_context (ctx : closure_context) =
		env <- Some ctx

	(* casting *)

	method expect_reference_type = jm#expect_reference_type

	method cast t =
		if follow t != t_dynamic then begin
			let vt = self#vtype t in
			jm#cast vt
		end else
			self#expect_reference_type

	method cast_expect ret t = match ret with
		| RValue (Some jsig) -> jm#cast jsig
		| _ -> self#cast t

	method tfunction e tf =
		let name = jc#get_next_closure_name in
		let outside = match Texpr.collect_captured_vars e with
			| [],false ->
				None
			| vl,accesses_this ->
				let vl = List.map (fun v -> v.v_id,v.v_name,jsignature_of_type v.v_type) vl in
				let vl = if accesses_this then (0,"this",jc#get_jsig) :: vl else vl in
				let ctx_class = create_context_class gctx jc jm name vl in
				Some ctx_class
		in
		let jsig =
			let args = List.map (fun (v,cto) ->
				if cto <> None then v.v_type <- self#mknull v.v_type;
				self#vtype v.v_type
			) tf.tf_args in
			let args = match outside with
				| None -> args
				| Some ctx_class -> ctx_class#get_jsig :: args
			in
			method_sig args (if ExtType.is_void (follow tf.tf_type) then None else Some (self#vtype tf.tf_type))
		in
		begin
			let jm = jc#spawn_method name jsig [MPublic;MStatic] in
			let handler = new texpr_to_jvm gctx jc jm tf.tf_type in
			begin match outside with
			| None -> ()
			| Some ctx_class ->
				handler#set_context ctx_class;
				let name = match ctx_class#get_args with
					| [(_,name),_] -> name
					| _ -> "_hx_ctx"
				in
				ignore(handler#add_named_local name ctx_class#get_jsig)
			end;
			let inits = List.map (fun (v,cto) ->
				let _,load,save = handler#add_local v VarArgument in
				match cto with
				| Some e when (match e.eexpr with TConst TNull -> false | _ -> true) ->
					let f () =
						load();
						let jsig = self#vtype v.v_type in
						jm#if_then
							(fun () -> jm#get_code#if_nonnull_ref jsig)
							(fun () ->
								handler#texpr (rvalue_sig jsig) e;
								jm#cast jsig;
								save();
							)
					in
					Some f
				| _ ->
					None
			) tf.tf_args in
			jm#finalize_arguments;
			List.iter (function
				| None -> ()
				| Some f -> f()
			) inits;
			handler#texpr RReturn tf.tf_expr;
		end;
		jm#read_closure true jc#get_this_path name jsig;
		outside

	(* access *)

	method read_native_array vta vte =
		NativeArray.read code vta vte

	method write_native_array vta vte =
		NativeArray.write code vta vte

	method read cast e1 fa =
		match fa with
		| FStatic({cl_path = (["java";"lang"],"Math")},({cf_name = "NaN" | "POSITIVE_INFINITY" | "NEGATIVE_INFINITY"} as cf)) ->
			jm#getstatic double_path cf.cf_name TDouble
		| FStatic({cl_path = (["java";"lang"],"Math")},({cf_name = "isNaN" | "isFinite"} as cf)) ->
			jm#read_closure true double_path cf.cf_name (jsignature_of_type cf.cf_type);
		| FStatic({cl_path = (["java";"lang"],"String")},({cf_name = "fromCharCode"} as cf)) ->
			jm#read_closure true (["haxe";"jvm"],"StringExt") cf.cf_name (jsignature_of_type cf.cf_type);
		| FStatic(c,({cf_kind = Method (MethNormal | MethInline)} as cf)) ->
			jm#read_closure true c.cl_path cf.cf_name (jsignature_of_type cf.cf_type);
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
		| FAnon ({cf_name = s} as cf) ->
			self#texpr rvalue_any e1;
			let default () =
				jm#string s;
				jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
				cast();
			in
			begin match follow e1.etype with
			| TAnon an ->
				let path,_ = TAnonIdentifiaction.identify gctx an.a_fields in
				code#dup;
				code#instanceof path;
				jm#if_then_else
					(fun () -> code#if_ref CmpEq)
					(fun () ->
						jm#cast (object_path_sig path);
						jm#getfield path s (self#vtype cf.cf_type);
						cast();
					)
					(fun () -> default());
			| _ ->
				default();
			end
		| FDynamic s | FInstance(_,_,{cf_name = s}) | FEnum(_,{ef_name = s}) | FClosure(Some({cl_interface = true},_),{cf_name = s}) | FClosure(None,{cf_name = s}) ->
			self#texpr rvalue_any e1;
			jm#string s;
			jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
			cast();
		| FClosure((Some(c,_)),cf) ->
			let jsig = self#vtype cf.cf_type in
			jm#read_closure false c.cl_path cf.cf_name jsig;
			self#texpr rvalue_any e1;
			jm#invokevirtual method_handle_path "bindTo" (method_sig [object_sig] (Some method_handle_sig));

	method read_write ret ak e (f : unit -> unit) =
		let apply dup =
			if ret <> RVoid && ak = AKPost then dup();
			f();
			if ret <> RVoid && ak <> AKPost then dup();
		in
		match (Texpr.skip e).eexpr with
		| TLocal v ->
			let _,load,store = self#get_local v in
			if ak <> AKNone then load();
			apply (fun () -> code#dup);
			store();
		| TField(_,FStatic(c,cf)) ->
			let jsig_cf = self#vtype cf.cf_type in
			if ak <> AKNone then jm#getstatic c.cl_path cf.cf_name jsig_cf;
			apply (fun () -> code#dup);
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
		| TField(e1,(FDynamic s | FAnon {cf_name = s} | FInstance(_,_,{cf_name = s}))) ->
			self#texpr rvalue_any e1;
			if ak <> AKNone then code#dup;
			jm#string s;
			if ak <> AKNone then begin
				code#dup_x1;
				jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
				self#cast_expect ret e.etype;
			end;
			apply (fun () -> code#dup_x2);
			self#cast (self#mknull e.etype);
			jm#invokestatic haxe_jvm_path "writeField" (method_sig [object_sig;string_sig;object_sig] None)
		| TArray(e1,e2) ->
			begin match follow e1.etype with
				| TInst({cl_path = (["haxe";"root"],"Array")} as c,[t]) ->
					let t = self#mknull t in
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
					self#cast t;
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
			assert false

	(* branching *)

	method apply_cmp = function
		| CmpNormal(op,_) -> (fun () -> code#if_ref op)
		| CmpSpecial f -> f

	method if_null t =
		(fun () -> code#if_null_ref t)

	method if_not_null t =
		(fun () -> code#if_nonnull_ref t)

	method condition e = match (Texpr.skip e).eexpr with
		| TBinop((OpEq | OpNotEq | OpLt | OpGt | OpLte | OpGte) as op,e1,e2) ->
			let op = convert_cmp_op op in
			self#binop_compare op e1 e2
		| _ ->
			self#texpr rvalue_any e;
			jm#cast TBool;
			CmpNormal(CmpEq,TBool)

	method switch ret e1 cases def =
		(* TODO: hack because something loses the exhaustiveness marker before we get here *)
		let is_exhaustive = OptimizerTexpr.is_exhaustive e1 || (ExtType.is_bool (follow e1.etype) && List.length cases > 1) in
		if cases = [] then
			self#texpr ret e1
		else if List.for_all is_const_int_pattern cases then begin
			let cases = List.map (fun (el,e) ->
				let il = List.map (fun e -> match e.eexpr with
					| TConst (TInt i32) -> i32
					| _ -> assert false
				) el in
				(il,(fun () -> self#texpr ret e))
			) cases in
			let def = match def with
				| None -> None
				| Some e -> Some (fun () -> self#texpr ret e)
			in
			self#texpr rvalue_any e1;
			jm#cast TInt;
			ignore(jm#int_switch is_exhaustive cases def);
		end else if List.for_all is_const_string_pattern cases then begin
			let cases = List.map (fun (el,e) ->
				let il = List.map (fun e -> match e.eexpr with
					| TConst (TString s) -> java_hash s
					| _ -> assert false
				) el in
				(il,(fun () -> self#texpr ret e))
			) cases in
			let def = match def with
				| None -> None
				| Some e -> Some (fun () -> self#texpr ret e)
			in
			self#texpr rvalue_any e1;
			jm#cast string_sig;
			let r = ref 0 in
			(* all strings can be null and we're not supposed to cause NPEs here... *)
			code#dup;
			jm#if_then
				(fun () -> jm#get_code#if_nonnull_ref string_sig)
				(fun () ->
					code#pop;
					r := code#get_fp;
					code#goto r
				);
			jm#invokevirtual string_path "hashCode" (method_sig [] (Some TInt));
			let r_default = jm#int_switch is_exhaustive cases def in
			r := r_default - !r;
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
					| [] -> assert false
					| [e] -> f e
					| e :: el ->
						List.fold_left (fun eacc e ->
							mk (TBinop(OpBoolOr,eacc,f e)) com.basic.tbool e.epos
						) (f e) el
				in
				(e_cond,e)
			) cases in
			(* If we rewrite an exhaustive switch that has no default value, treat the last case as the default case to satisfy control flow. *)
			let cases,def = if is_exhaustive && def = None then (match List.rev cases with (_,e) :: cases -> List.rev cases,Some e | _ -> assert false) else cases,def in
			let e = List.fold_left (fun e_else (e_cond,e_then) -> Some (mk (TIf(e_cond,e_then,e_else)) e_then.etype e_then.epos)) def el in
			self#texpr ret (Option.get e);
			pop_scope()
		end

	(* binops *)

	method binop_exprs cast_type f1 f2 =
		f1();
		jm#cast ~allow_to_string:true cast_type;
		f2();
		jm#cast ~allow_to_string:true cast_type;

	method get_binop_type_sig jsig1 jsig2 = match jsig1,jsig2 with
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

	method get_binop_type t1 t2 = self#get_binop_type_sig (jsignature_of_type t1) (jsignature_of_type t2)

	method do_compare op =
		match code#get_stack#get_stack_items 2 with
		| [TInt | TByte | TChar | TBool;TInt | TByte | TChar | TBool] ->
			let op = flip_cmp_op op in
			CmpSpecial (fun () -> code#if_icmp_ref op)
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
			CmpSpecial (fun () -> (if op = CmpEq then code#if_acmp_ne_ref else code#if_acmp_eq_ref) t1 t2)
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
		let sig1 = jsignature_of_type e1.etype in
		let sig2 = jsignature_of_type e2.etype in
		match (Texpr.skip e1),(Texpr.skip e2) with
		| {eexpr = TConst TNull},_ when not (is_unboxed sig2) ->
			self#texpr rvalue_any e2;
			CmpSpecial ((if op = CmpEq then self#if_not_null else self#if_null) sig2)
		| _,{eexpr = TConst TNull} when not (is_unboxed sig1) ->
			self#texpr rvalue_any e1;
			CmpSpecial ((if op = CmpEq then self#if_not_null else self#if_null) sig1)
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
				let f e () = self#texpr rvalue_any e in
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
						(self#if_not_null sig1)
						(fun () ->
							jm#get_code#pop;
							self#texpr rvalue_any e2;
							self#boolop (CmpSpecial (self#if_not_null sig2))
						)
						(fun () ->
							jm#cast ~not_null:true cast_type;
							self#texpr rvalue_any e2;
							jm#get_code#dup;
							jm#if_then_else
								(self#if_not_null sig2)
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
					(self#if_not_null sig1)
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
					(self#if_not_null sig2)
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
			| _ -> assert false
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
					f1();
					jm#cast TDouble;
					f2();
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
					f1();
					jm#cast TLong;
					f2();
					jm#cast TInt;
					code#lshl;
				| OpShr ->
					f1();
					jm#cast TLong;
					f2();
					jm#cast TInt;
					code#lshr;
				| OpUShr ->
					f1();
					jm#cast TLong;
					f2();
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
						f();
						jm#cast TBool;
					in
					operand f1;
					jm#if_then_else
						(fun () -> code#if_ref CmpEq)
						(fun () -> operand f2)
						(fun () -> code#bconst false)
				| OpBoolOr ->
					let operand f =
						f();
						jm#cast TBool;
					in
					operand f1;
					jm#if_then_else
						(fun () -> code#if_ref CmpEq)
						(fun () -> code#bconst true)
						(fun () -> operand f2)
				| _ ->
					emit_exprs();
					let name = method_name () in
					jm#invokestatic haxe_jvm_path name (method_sig [object_sig;object_sig] (Some object_sig))
				end
			| TObject(path,_) ->
				emit_exprs();
				if path = string_path then
					jm#invokestatic haxe_jvm_path "stringConcat" (method_sig [object_sig;object_sig] (Some string_sig))
				else begin
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

	method binop ret op e1 e2 = match op with
		| OpEq | OpNotEq | OpLt | OpGt | OpLte | OpGte ->
			let op = convert_cmp_op op in
			self#boolop (self#binop_compare op e1 e2)
		| OpAssign ->
			let f () =
				self#texpr (rvalue_type e1.etype) e2;
				self#cast e1.etype;
			in
			self#read_write ret AKNone e1 f
		| OpAssignOp op ->
			let jsig1 = jsignature_of_type e1.etype in
			begin match op,(Texpr.skip e1).eexpr,(Texpr.skip e2).eexpr with
			| OpAdd,TLocal v,TConst (TInt i32) when ExtType.is_int v.v_type && in_range false Int8Range (Int32.to_int i32) && self#var_slot_is_in_int8_range v->
				let slot,load,_ = self#get_local v in
				let i = Int32.to_int i32 in
				code#iinc slot i;
				if ret <> RVoid then load();
			| OpSub,TLocal v,TConst (TInt i32) when ExtType.is_int v.v_type && in_range false Int8Range (-Int32.to_int i32) && self#var_slot_is_in_int8_range v ->
				let slot,load,_ = self#get_local v in
				let i = -Int32.to_int i32 in
				code#iinc slot i;
				if ret <> RVoid then load();
			| _ ->
				let f () =
					self#binop_basic ret op (self#get_binop_type e1.etype e2.etype) (fun () -> ()) (fun () -> self#texpr rvalue_any e2);
					jm#cast jsig1;
				in
				self#read_write ret AKPre e1 f
			end
		| _ ->
			let f e () = self#texpr rvalue_any e in
			self#binop_basic ret op (self#get_binop_type e1.etype e2.etype) (f e1) (f e2)

	method unop ret op flag e =
		match op,(Texpr.skip e).eexpr with
		| (Increment | Decrement),TLocal v when ExtType.is_int v.v_type && self#var_slot_is_in_int8_range v ->
			let slot,load,_ = self#get_local v in
			if flag = Postfix && ret <> RVoid then load();
			code#iinc slot (if op = Increment then 1 else -1);
			if flag = Prefix && ret <> RVoid then load();
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
			let jsig = jsignature_of_type (follow e.etype) in
			jm#cast jsig;
			begin match jsig with
			| TLong -> code#lneg;
			| TDouble -> code#dneg;
			| TByte | TShort | TInt -> code#ineg;
			| _ -> jm#invokestatic haxe_jvm_path "opNeg" (method_sig [object_sig] (Some object_sig))
			end;
			self#cast e.etype;
		| Not,_ ->
			jm#if_then_else
				(self#apply_cmp (self#condition e))
				(fun () -> code#bconst false)
				(fun () -> code#bconst true)
		| NegBits,_ ->
			let jsig = jsignature_of_type (follow e.etype) in
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

	method get_argument_signatures t el =
		match jsignature_of_type t with
		| TMethod(jsigs,r) -> jsigs,r
		| _ -> List.map (fun _ -> object_sig) el,(Some object_sig)

	method call_arguments t el =
		let tl,tr = self#get_argument_signatures t el in
		let varargs_type = match follow t with
			| TFun(tl,_) ->
				begin match List.rev tl with
				| (_,_,(TAbstract({a_path = ["haxe";"extern"],"Rest"},[t]))) :: _ -> Some (jsignature_of_type t)
				| _ -> None
				end
			| _ ->
				None
		in
		let rec loop acc tl el = match tl,el with
			| jsig :: tl,e :: el ->
				begin match tl,varargs_type with
				| [],Some jsig' ->
					self#new_native_array jsig' (e :: el);
					List.rev (jsig :: acc)
				| _ ->
					self#texpr (rvalue_sig jsig) e;
					jm#cast jsig;
					loop (jsig :: acc) tl el
				end
			| _,[] -> List.rev acc
			| [],e :: el ->
				(* TODO: this sucks *)
				self#texpr rvalue_any e;
				loop (self#vtype e.etype :: acc) [] el
		in
		let tl = loop [] tl el in
		tl,tr

	method call ret tr e1 el =
		let retype tr = match tr with None -> [] | Some t -> [t] in
		let tro = match (Texpr.skip e1).eexpr with
		| TField(_,FStatic({cl_path = ["haxe";"jvm"],"Jvm"},({cf_name = "referenceEquals"} as cf))) ->
			let tl,tr = self#call_arguments cf.cf_type el in
			begin match tl with
				| [t1;t2] -> self#boolop (CmpSpecial (fun () -> code#if_acmp_ne_ref t1 t2))
				| _ -> assert false
			end;
			tr
		| TField(_,FStatic({cl_path = ["haxe";"jvm"],"Jvm"},({cf_name = "instanceof"}))) ->
			begin match el with
				| [e1;{eexpr = TTypeExpr mt;epos = pe}] ->
					self#texpr rvalue_any e1;
					self#expect_reference_type;
					let path = match jsignature_of_type (type_of_module_type mt) with
						| TObject(path,_) -> path
						| _ -> Error.error "Class expected" pe
					in
					code#instanceof path;
					Some TBool
				| _ -> Error.error "Type expression expected" e1.epos
			end;
		| TField(_,FStatic({cl_path = ["haxe";"jvm"],"Jvm"},({cf_name = "invokedynamic"}))) ->
			begin match el with
				| e_bsm :: {eexpr = TConst (TString name)} :: {eexpr = TArrayDecl el_static_args} :: el ->
					let t = tfun (List.map (fun e -> e.etype) el) tr in
					let tl,tr = self#call_arguments t el in
					let path,mname = match e_bsm.eexpr with
						| TField(_,FStatic(c,cf)) -> c.cl_path,cf.cf_name
						| _ -> Error.error "Reference to bootstrap method expected" e_bsm.epos
					in
					let rec loop consts jsigs static_args = match static_args with
						| e :: static_args ->
							let const,jsig =  match e.eexpr with
							| TConst (TString s) -> pool#add_const_string s,string_sig
							| TConst (TInt i) -> pool#add (ConstInt i),TInt
							| TConst (TFloat f) -> pool#add (ConstDouble (float_of_string f)),TDouble
							| TField(_,FStatic(c,cf)) ->
								let offset = pool#add_field c.cl_path cf.cf_name (self#vtype cf.cf_type) FKMethod in
								pool#add (ConstMethodHandle(6, offset)),method_handle_sig
							| _ -> Error.error "Invalid static argument" e.epos
							in
							loop (const :: consts) (jsig :: jsigs) static_args
						| [] ->
							List.rev consts,List.rev jsigs
					in
					let consts,jsigs = loop [] [] el_static_args in
					let mtl = method_lookup_sig :: string_sig :: method_type_sig :: jsigs in
					let index = jc#get_bootstrap_method path mname (method_sig mtl (Some call_site_sig)) consts in
					let jsig_method = method_sig tl tr in
					let offset_info = pool#add_name_and_type name jsig_method FKMethod in
					let offset = pool#add (ConstInvokeDynamic(index,offset_info)) in
					code#invokedynamic offset tl (retype tr);
					tr
				| _ ->
					Error.error "Bad invokedynamic call" e1.epos
			end
		| TField(_,FStatic({cl_path = (["java";"lang"],"Math")},{cf_name = ("isNaN" | "isFinite") as name})) ->
			begin match el with
			| [e1] ->
				self#texpr rvalue_any e1;
				jm#cast TDouble;
				jm#invokestatic (["java";"lang"],"Double") name (method_sig [TDouble] (Some TBool));
				Some TBool
			| _ ->
				assert false
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
				assert false
			end;
		| TField(_,FStatic({cl_path = (["java";"lang"],"Math")} as c,({cf_name = ("ffloor" | "fceil")} as cf))) ->
			let tl,tr = self#call_arguments cf.cf_type el in
			jm#invokestatic c.cl_path (String.sub cf.cf_name 1 (String.length cf.cf_name - 1)) (method_sig tl tr);
			tr
		| TField(_,FStatic({cl_path = (["haxe";"_Int64"],"Int64_Impl_")},{cf_name = "make"})) ->
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
				assert false
			end
		| TIdent "__array__" | TField(_,FStatic({cl_path = (["java"],"NativeArray")},{cf_name = "make"})) ->
			begin match follow tr with
			| TInst({cl_path = (["java"],"NativeArray")},[t]) ->
				let jsig = self#vtype t in
				self#new_native_array jsig el;
				Some (array_sig jsig)
			| _ ->
				Error.error (Printf.sprintf "Bad __array__ type: %s" (s_type (print_context()) tr)) e1.epos;
			end
		| TField(e1,FStatic(c,({cf_kind = Method (MethNormal | MethInline)} as cf))) ->
			let tl,tr = self#call_arguments cf.cf_type el in
			jm#invokestatic c.cl_path cf.cf_name (method_sig tl tr);
			tr
		| TField(e1,FInstance(c,tl,({cf_kind = Method (MethNormal | MethInline)} as cf))) ->
			let is_super = match e1.eexpr with
			| TConst TSuper ->
				code#aload jc#get_jsig 0;
				true
			| _ ->
				self#texpr rvalue_any e1;
				false
			in
			begin match find_overload_rec false (apply_params c.cl_params tl) c cf el with
			| None -> Error.error "Could not find overload" e1.epos
			| Some(c,cf) ->
				let tl,tr = self#call_arguments cf.cf_type el in
				(if is_super then jm#invokespecial else if c.cl_interface then jm#invokeinterface else jm#invokevirtual) c.cl_path cf.cf_name (self#vtype cf.cf_type);
				tr
			end
		| TField(_,FEnum(en,ef)) ->
			let tl,_ = self#call_arguments ef.ef_type el in
			let tr = self#vtype tr in
			jm#invokestatic en.e_path ef.ef_name (method_sig tl (Some tr));
			Some tr
		| TConst TSuper ->
			let c,cf = match gctx.current_field_info with
				| Some ({super_call_fields = hd :: tl} as info) ->
					info.super_call_fields <- tl;
					hd
				| _ ->
					Error.error "Something went wrong" e1.epos
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
				| _ -> assert false
			end
		| _ ->
			let rec has_unknown_args jsig =
				is_dynamic_at_runtime jsig || match jsig with
					| TMethod(jsigs,_) -> List.exists has_unknown_args jsigs
					| _ -> false
			in
			if has_unknown_args (jsignature_of_type e1.etype) then begin
				self#texpr rvalue_any e1;
				jm#cast method_handle_sig;
				self#new_native_array object_sig el;
				jm#invokestatic haxe_jvm_path "call" (method_sig [method_handle_sig;array_sig object_sig] (Some object_sig));
				Some object_sig
			end else begin
				self#texpr rvalue_any e1;
				jm#cast method_handle_sig;
				let tl,tr = self#call_arguments e1.etype el in
				jm#invokevirtual method_handle_path "invoke" (method_sig tl tr);
				tr
			end
		in
		match ret = RVoid,tro with
		| true,Some _ -> code#pop
		| true,None -> ()
		| false,Some _ -> self#cast tr;
		| false,None -> assert false

	(* exceptions *)

	method throw vt =
		jm#expect_reference_type;
		jm#invokestatic (["haxe";"jvm"],"Exception") "wrap" (method_sig [object_sig] (Some exception_sig));
		code#athrow;
		jm#set_terminated true

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
		let unwrap () =
			code#dup;
			code#instanceof haxe_exception_path;
			jm#if_then_else
				(fun () -> code#if_ref CmpEq)
				(fun () ->
					jm#cast haxe_exception_sig;
					jm#getfield (["haxe";"jvm"],"Exception") "value" object_sig;
				)
				(fun () -> jm#cast object_sig);
		in
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
			jm#get_code#dup;
			jm#invokestatic haxe_exception_path "setException" (method_sig [throwable_sig] None);
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
			start_exception_block exc#get_native_exception_path exc#get_native_exception_type;
			if not exc#is_native_exception then begin
				unwrap();
				self#cast v.v_type
			end;
			let term = run_catch_expr v e in
			let r = jm#maybe_make_jump in
			term,r
		in
		let commit_instanceof_checks excl =
			start_exception_block throwable_path throwable_sig;
			let pop_scope = jm#push_scope in
			let _,load,save = jm#add_local "exc" throwable_sig VarWillInit in
			code#dup;
			save();
			unwrap();
			let restore = jm#start_branch in
			let rl = ref [] in
			let rec loop excl = match excl with
				| [] ->
					code#pop;
					load();
					code#athrow;
					jm#set_terminated true
				| (_,v,e) :: excl ->
					code#dup;
					let path = match self#vtype (self#mknull v.v_type) with TObject(path,_) -> path | _ -> assert false in
					if path = object_path then begin
						code#pop;
						restore();
						let term = run_catch_expr v e in
						rl := (term,ref 0) :: !rl;
					end else begin
						code#instanceof path;
						jm#if_then_else
							(fun () -> code#if_ref CmpEq)
							(fun () ->
								restore();
								self#cast v.v_type;
								let term = run_catch_expr v e in
								rl := (term,ref 0) :: !rl;
							)
							(fun () -> loop excl)
					end
			in
			loop excl;
			pop_scope();
			!rl
		in
		let rec loop acc excl = match excl with
			| (exc,v,e) :: excl ->
				if List.exists (fun (exc',_,_) -> exc'#is_assignable_to exc) excl || excl = [] && not exc#is_native_exception then begin
					let res = commit_instanceof_checks ((exc,v,e) :: excl) in
					acc @ res
				end else begin
					let res = add_catch (exc,v,e) in
					loop (res :: acc) excl
				end
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
		| Type.TInt i32 -> code#iconst i32
		| TFloat f ->
			begin match ret with
			| RValue (Some (TFloat | TObject((["java";"lang"],"Float"),_))) -> code#fconst (float_of_string f)
			| _ -> code#dconst (float_of_string f)
			end
		| TBool true -> code#bconst true
		| TBool false -> code#bconst false
		| TNull -> code#aconst_null (self#vtype t)
		| TThis ->
			let _,load,_ = self#get_local_by_id (0,"this") in
			load()
		| TString s -> jm#string s
		| TSuper -> failwith "Invalid super access"

	method new_native_array jsig el =
		jm#new_native_array jsig (List.map (fun e -> fun () -> self#texpr (rvalue_sig jsig) e) el)

	method basic_type_path name =
		let offset = pool#add_field (["java";"lang"],name) "TYPE" java_class_sig FKField in
		code#getstatic offset java_class_sig

	method type_expr = function
		| TByte -> self#basic_type_path "Byte"
		| TChar -> self#basic_type_path "Character"
		| TDouble -> self#basic_type_path "Double"
		| TFloat -> self#basic_type_path "Float"
		| TInt -> self#basic_type_path "Integer"
		| TLong -> self#basic_type_path "Long"
		| TShort -> self#basic_type_path "Short"
		| TBool -> self#basic_type_path "Boolean"
		| TObject(path,_) ->
			let offset = pool#add_path path in
			let t = object_path_sig path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,t)]))
		| TMethod _ ->
			let offset = pool#add_path method_handle_path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,method_handle_sig)]))
		| TTypeParameter _ ->
			let offset = pool#add_path object_path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,object_sig)]))
		| TArray _ as t ->
			(* TODO: this seems hacky *)
			let offset = pool#add_path ([],generate_signature false t) in
			code#ldc offset (TObject(java_class_path,[TType(WNone,object_sig)]))
		| jsig ->
			print_endline (generate_signature false jsig);
			assert false

	method texpr ret e =
		try
			if not jm#is_terminated then self#texpr' ret e
		with Failure s ->
			raise (HarderFailure (Printf.sprintf "Expr %s\n%s" (s_expr_pretty false "" false (s_type (print_context())) e) s))

	method texpr' ret e =
		code#set_line (Lexer.get_error_line e.epos);
		match e.eexpr with
		| TVar(v,Some e1) ->
			self#texpr (rvalue_type v.v_type) e1;
			self#cast v.v_type;
			let _,_,store = self#add_local v VarWillInit in
			store()
		| TVar(v,None) ->
			ignore(self#add_local v VarNeedDefault);
		| TLocal _ | TConst _  | TTypeExpr _ when ret = RVoid ->
			()
		| TLocal v ->
			let _,load,_ = self#get_local v in
			load()
		| TTypeExpr mt ->
			self#type_expr (jsignature_of_type (type_of_module_type mt))
		| TUnop(op,flag,e1) ->
			begin match op with
			| Not | Neg | NegBits when ret = RVoid -> self#texpr ret e1
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
			| _ when ret = RVoid ->
				self#texpr ret e1;
				self#texpr ret e2;
			| _ ->
				self#binop ret op e1 e2
			end
		| TConst ct ->
			self#const ret e.etype ct
		| TIf(e1,e2,None) ->
			jm#if_then
				(self#apply_cmp (self#condition e1))
				(fun () -> self#texpr RVoid (mk_block e2))
		| TIf(e1,e2,Some e3) ->
			jm#if_then_else
				(self#apply_cmp (self#condition e1))
				(fun () ->
					self#texpr ret (mk_block e2);
					if ret <> RVoid then self#cast e.etype
				)
				(fun () ->
					self#texpr ret (mk_block e3);
					if ret <> RVoid then self#cast e.etype;
				)
		| TSwitch(e1,cases,def) ->
			self#switch ret e1 cases def
		| TWhile(e1,e2,flag) -> (* TODO: do-while *)
			(* TODO: could optimize a bit *)
			block_exits <- ExitLoop :: block_exits;
			let is_true_loop = match (Texpr.skip e1).eexpr with TConst (TBool true) -> true | _ -> false in
			jm#add_stack_frame;
			let fp = code#get_fp in
			let old_continue = continue in
			continue <- fp;
			let old_breaks = breaks in
			breaks <- [];
			let restore = jm#start_branch in
			let jump_then = if not is_true_loop then self#apply_cmp (self#condition e1) () else ref 0 in
			let pop_scope = jm#push_scope in
			self#texpr RVoid e2;
			if not jm#is_terminated then code#goto (ref (fp - code#get_fp));
			pop_scope();
			restore();
			if not is_true_loop || breaks <> [] then begin
				jump_then := code#get_fp - !jump_then;
				let fp' = code#get_fp in
				List.iter (fun r -> r := fp' - !r) breaks;
				jm#add_stack_frame
			end else
				jm#set_terminated true;
			continue <- old_continue;
			breaks <- old_breaks;
			block_exits <- List.tl block_exits;
		| TBreak ->
			self#emit_block_exits true;
			let r = ref (code#get_fp) in
			code#goto r;
			breaks <- r :: breaks;
			jm#set_terminated true;
		| TContinue ->
			self#emit_block_exits true;
			code#goto (ref (continue - code#get_fp));
			jm#set_terminated true;
		| TTry(e1,catches) ->
			self#try_catch ret e1 catches
		| TField(e1,fa) ->
			if ret = RVoid then self#texpr ret e1
			else self#read (fun () -> self#cast_expect ret e.etype) e1 fa;
		| TCall(e1,el) ->
			self#call ret e.etype e1 el
		| TNew({cl_path = (["java"],"NativeArray")},[t],[e1]) ->
			self#texpr (match ret with RVoid -> RVoid | _ -> rvalue_any) e1;
			(* Technically this could throw... but whatever *)
			if ret <> RVoid then ignore(NativeArray.create jm#get_code jc#get_pool (jsignature_of_type t))
		| TNew(c,tl,el) ->
			begin match get_constructor (fun cf -> cf.cf_type) c with
			|_,cf ->
				begin match find_overload_rec true (apply_params c.cl_params tl) c cf el with
				| None -> Error.error "Could not find overload" e.epos
				| Some (c',cf) ->
					let f () =
						let tl,_ = self#call_arguments  cf.cf_type el in
						tl
					in
					jm#construct ~no_value:(if ret = RVoid then true else false) (get_construction_mode c' cf) c.cl_path f
				end
			end
		| TReturn None ->
			self#emit_block_exits false;
			code#return_void;
			jm#set_terminated true;
		| TReturn (Some e1) ->
			self#texpr rvalue_any e1;
			self#cast return_type;
			let vt = self#vtype return_type in
			self#emit_block_exits false;
			code#return_value vt;
			jm#set_terminated true;
		| TFunction tf ->
			begin match self#tfunction e tf with
			| None ->
				()
			| Some ctx_class ->
				begin match ctx_class#get_args with
				| [(arg,jsig)] ->
					let _,load,_ = self#get_local_by_id arg in
					load();
					self#expect_reference_type;
					jm#invokevirtual method_handle_path "bindTo" (method_sig [object_sig] (Some method_handle_sig));
				| args ->
					let f () =
						let tl = List.map (fun (arg,jsig) ->
							let _,load,_ = self#get_local_by_id arg in
							load();
							jm#cast jsig;
							jsig
						) args in
						tl
					in
					jm#construct ConstructInit ctx_class#get_path f;
					jm#invokevirtual method_handle_path "bindTo" (method_sig [object_sig] (Some method_handle_sig));
				end
			end
		| TArrayDecl el when ret = RVoid ->
			List.iter (self#texpr ret) el
		| TArrayDecl el ->
			begin match follow e.etype with
			| TInst({cl_path = (["haxe";"root"],"Array")},[t]) ->
				self#new_native_array (jsignature_of_type (self#mknull t)) el;
				jm#invokestatic (["haxe";"root"],"Array") "ofNative" (method_sig [array_sig object_sig] (Some (object_path_sig (["haxe";"root"],"Array"))));
				self#cast e.etype
			| _ ->
				assert false
			end
		| TArray(e1,e2) when ret = RVoid ->
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
				let vte = self#vtype t in
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
			if ret = RReturn && not jm#is_terminated then code#return_void;
		| TBlock el ->
			let rec loop el = match el with
				| [] -> assert false
				| [e1] ->
					self#texpr (if ret = RReturn then RVoid else ret) e1;
					if ret = RReturn && not jm#is_terminated then code#return_void;
				| e1 :: el ->
					self#texpr RVoid e1;
					loop el
			in
			let pop_scope = jm#push_scope in
			loop el;
			pop_scope();
		| TCast(e1,None) ->
			self#texpr ret e1;
			if ret <> RVoid then self#cast e.etype
		| TCast(e1,Some mt) ->
			self#texpr rvalue_any e1;
			let jsig = jsignature_of_type (type_of_module_type mt) in
			if is_unboxed jsig || is_unboxed jm#get_code#get_stack#top then jm#cast jsig
			else code#checkcast (t_infos mt).mt_path;
			if ret = RVoid then code#pop;
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
				| _ -> assert false
			in
			let cpath = ((fst path),Printf.sprintf "%s$%s" (snd path) ef.ef_name) in
			let jsig = (object_path_sig cpath) in
			jm#cast jsig;
			jm#getfield cpath name jsig_arg;
			self#cast e.etype;
		| TThrow e1 ->
			self#texpr rvalue_any e1;
			let exc = new haxe_exception gctx e1.etype in
			if not (List.exists (fun exc' -> exc#is_assignable_to exc') caught_exceptions) then jm#add_thrown_exception exc#get_native_exception_path;
			if not exc#is_native_exception then begin
				let vt = self#vtype (self#mknull e1.etype) in
				self#throw vt
			end else begin
				code#athrow;
				jm#set_terminated true
			end
		| TObjectDecl fl ->
			begin match follow e.etype with
			(* The guard is here because in the case of quoted fields like `"a-b"`, the field is not part of the
			   type. In this case we have to do full dynamic construction. *)
			| TAnon an when List.for_all (fun ((name,_,_),_) -> PMap.mem name an.a_fields) fl ->
				let path,fl' = TAnonIdentifiaction.identify gctx an.a_fields in
				jm#construct ConstructInit path (fun () ->
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
					List.iter (fun (_,load) ->
						load();
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
			Error.error (s_expr_ast false "" (s_type (print_context())) e) e.epos;

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

let failsafe p f =
	try
		f ()
	with Failure s | HarderFailure s ->
		Error.error s p

let generate_dynamic_access gctx (jc : JvmClass.builder) fields is_anon =
	begin match fields with
	| [] ->
		()
	| _ ->
		let jsig = method_sig [string_sig] (Some object_sig) in
		let jm = jc#spawn_method "_hx_getField" jsig [MPublic;MSynthetic] in
		let _,load,_ = jm#add_local "name" string_sig VarArgument in
		jm#finalize_arguments;
		load();
		jm#invokevirtual string_path "hashCode" (method_sig [] (Some TInt));
		let cases = List.map (fun (name,jsig,kind) ->
			let hash = java_hash name in
			[hash],(fun () ->
				begin match kind with
					| Method (MethNormal | MethInline) ->
						jm#read_closure false jc#get_this_path name jsig;
						jm#load_this;
						jm#invokevirtual method_handle_path "bindTo" (method_sig [object_sig] (Some method_handle_sig));
					| _ ->
						jm#load_this;
						jm#getfield jc#get_this_path name jsig;
						jm#expect_reference_type;
				end;
				ignore(jm#get_code#get_stack#pop);
				jm#get_code#get_stack#push object_sig;
			)
		) fields in
		let def = (fun () ->
			jm#load_this;
			load();
			jm#invokespecial jc#get_super_path "_hx_getField" jsig;
		) in
		ignore(jm#int_switch false cases (Some def));
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
						(fun () -> jm#get_code#if_null_ref boolean_sig)
						(fun () ->
							def();
						)
				end;
			)
		) fields in
		ignore(jm#int_switch false cases (Some def));
		jm#return
	end

class tclass_to_jvm gctx c = object(self)
	val is_annotation = Meta.has Meta.Annotation c.cl_meta
	val field_inits = DynArray.create ()
	val delayed_field_inits = DynArray.create ()

	val jc = new JvmClass.builder c.cl_path (match c.cl_super with
		| Some(c,_) -> c.cl_path
		| None ->
			if c.cl_interface || Meta.has Meta.NativeGen c.cl_meta then object_path else haxe_object_path
		)

	method private set_access_flags =
		jc#add_access_flag 1; (* public *)
		if c.cl_final then jc#add_access_flag 0x10;
		if c.cl_interface then begin
			jc#add_access_flag 0x200;
			jc#add_access_flag 0x400;
		end;
		if is_annotation then begin
			jc#add_access_flag 0x2000;
			jc#add_interface (["java";"lang";"annotation"],"Annotation");
			(* TODO: this should be done via Haxe metadata instead of hardcoding it here *)
			jc#add_annotation retention_path ["value",(AEnum(retention_policy_sig,"RUNTIME"))];
		end;
		if c.cl_path = (["haxe";"jvm"],"Enum") then jc#add_access_flag 0x4000; (* enum *)

	method private handle_relation_type_params =
		let map_type_params t =
			let has_type_param = ref false in
			let rec loop t = match follow t with
				| TInst({cl_kind = KTypeParameter tl},_) ->
					has_type_param := true;
					begin match tl with
					| [t] -> t
					| _ -> t_dynamic
					end
				| _ -> Type.map loop t
			in
			let t = match follow t with
				| TFun(tl,tr) ->
					let tl = List.map (fun (n,o,t) -> n,o,loop t) tl in
					let tr = loop tr in
					TFun(tl,tr)
				| _ ->
					assert false
			in
			if !has_type_param then Some t else None
		in
		let make_bridge cf_impl t =
			let jsig = jsignature_of_type t in
			if not (jc#has_method cf_impl.cf_name jsig) then begin
				begin match follow t with
				| TFun(tl,tr) ->
					let jm = jc#spawn_method cf_impl.cf_name jsig [MPublic;MSynthetic;MBridge] in
					jm#load_this;
					let jsig_impl = jsignature_of_type cf_impl.cf_type in
					let jsigs,_ = match jsig_impl with TMethod(jsigs,jsig) -> jsigs,jsig | _ -> assert false in
					List.iter2 (fun (n,_,t) jsig ->
						let _,load,_ = jm#add_local n (jsignature_of_type t) VarArgument in
						load();
						jm#cast jsig;
					) tl jsigs;
					jm#invokevirtual c.cl_path cf_impl.cf_name jsig_impl;
					if not (ExtType.is_void (follow tr)) then jm#cast (jsignature_of_type tr);
					jm#return;
				| _ ->
					()
				end
			end
		in
		let check is_interface cf cf_impl =
			match map_type_params cf.cf_type with
			| Some t ->
				make_bridge cf_impl t
			| None ->
				(* If we implement an interface with variance, we need a bridge method too (#8528). *)
				if is_interface && not (type_iseq cf.cf_type cf_impl.cf_type) then make_bridge cf_impl cf.cf_type
		in
		let check is_interface cf cf_impl =
			check is_interface cf cf_impl;
			(* TODO: I think this is incorrect... have to investigate though *)
			(* List.iter (fun cf -> check is_interface cf cf_impl) cf.cf_overloads *)
		in
		let rec loop map_type c_int =
			List.iter (fun (c_int,tl) ->
				let map_type t = apply_params c_int.cl_params tl (map_type t) in
				List.iter (fun cf ->
					match cf.cf_kind,raw_class_field (fun cf -> map_type cf.cf_type) c (List.map snd c.cl_params) cf.cf_name with
					| (Method (MethNormal | MethInline)),(Some(c',_),_,cf_impl) when c' == c ->
						let tl = match follow (map_type cf.cf_type) with
							| TFun(tl,_) -> tl
							| _ -> assert false
						in
						begin match find_overload_rec' false map_type c cf.cf_name (List.map (fun (_,_,t) -> Texpr.Builder.make_null t null_pos) tl) with
							| Some(_,cf_impl) -> check true cf cf_impl
							| None -> ()
						end;
					| _ ->
						()
				) c_int.cl_ordered_fields;
				loop map_type c_int
			) c_int.cl_implements
		in
		loop (fun t -> t) c;
		begin match c.cl_overrides,c.cl_super with
		| [],_ ->
			()
		| fields,Some(c_sup,tl) ->
			List.iter (fun cf_impl ->
				match cf_impl.cf_kind,raw_class_field (fun cf -> apply_params c_sup.cl_params tl cf.cf_type) c_sup tl cf_impl.cf_name with
				| (Method (MethNormal | MethInline)),(Some(c,tl),_,cf) -> check false cf cf_impl
				| _ -> ()
			) fields
		| _ ->
			assert false
		end

	method private set_interfaces =
		List.iter (fun (c_int,tl) ->
			if is_annotation && c_int.cl_path = (["java";"lang";"annotation"],"Annotation") then
				()
			else begin
				jc#add_interface c_int.cl_path
			end
		) c.cl_implements

	method private generate_empty_ctor =
		let jsig_empty = method_sig [haxe_empty_constructor_sig] None in
		let jm_empty_ctor = jc#spawn_method "<init>" jsig_empty [MPublic] in
		let _,load,_ = jm_empty_ctor#add_local "_" haxe_empty_constructor_sig VarArgument in
		jm_empty_ctor#load_this;
		if c.cl_constructor = None then begin
			let handler = new texpr_to_jvm gctx jc jm_empty_ctor gctx.com.basic.tvoid in
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
			let handler = new texpr_to_jvm gctx jc jm_empty_ctor gctx.com.basic.tvoid in
			DynArray.iter (fun e ->
				handler#texpr RVoid e;
			) delayed_field_inits;
		end;
		jm_empty_ctor#get_code#return_void;

	method private generate_implicit_ctors =
		try
			let sm = Hashtbl.find gctx.implicit_ctors c.cl_path in
			PMap.iter (fun _ (c,cf) ->
				let cmode = get_construction_mode c cf in
				let jm = jc#spawn_method (if cmode = ConstructInit then "<init>" else "new") (jsignature_of_type cf.cf_type) [MPublic] in
				let handler = new texpr_to_jvm gctx jc jm gctx.com.basic.tvoid in
				jm#load_this;
				DynArray.iter (fun e ->
					handler#texpr RVoid e;
				) field_inits;
				let tl = match follow cf.cf_type with TFun(tl,_) -> tl | _ -> assert false in
				List.iter (fun (n,_,t) ->
					let _,load,_ = jm#add_local n (jsignature_of_type t) VarArgument in
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
				tf.tf_expr,tf.tf_args,tf.tf_type
			| _ ->
				e,[],t_dynamic
		in
		let handler = new texpr_to_jvm gctx jc jm tr in
		List.iter (fun (v,_) ->
			ignore(handler#add_local v VarArgument);
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
		gctx.current_field_info <- get_field_info gctx cf.cf_meta;
		let jsig = jsignature_of_type cf.cf_type in
		let flags = [MPublic] in
		let flags = if c.cl_interface then MAbstract :: flags else flags in
		let flags = if mtype = MStatic then MethodAccessFlags.MStatic :: flags else flags in
		let flags = if has_class_field_flag cf CfFinal then MFinal :: flags else flags in
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
				let ssig = generate_method_signature true (jsignature_of_type cf.cf_type) in
				let s = if cf.cf_params = [] then ssig else Printf.sprintf "<%s>%s" stl ssig in
				let offset = jc#get_pool#add_string s in
				jm#add_attribute (AttributeSignature offset);
		end;
		AnnotationHandler.generate_annotations (jm :> JvmBuilder.base_builder) cf.cf_meta;

	method generate_field gctx (jc : JvmClass.builder) c mtype cf =
		let jsig = jsignature_of_type cf.cf_type in
		let flags = [FdPublic] in
		let flags = if mtype = MStatic then FdStatic :: flags else flags in
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
		let ssig = generate_signature true (jsignature_of_type cf.cf_type) in
		let offset = jc#get_pool#add_string ssig in
		jm#add_attribute (AttributeSignature offset)

		method generate_main =
			let jsig = method_sig [array_sig string_sig] None in
			let jm = jc#spawn_method "main" jsig [MPublic;MStatic] in
			let _,load,_ = jm#add_local "args" (TArray(string_sig,None)) VarArgument in
			if has_feature gctx.com "haxe.root.Sys.args" then begin
				load();
				jm#putstatic (["haxe";"root"],"Sys") "_args" (TArray(string_sig,None))
			end;
			jm#invokestatic (["haxe"; "java"], "Init") "init" (method_sig [] None);
			jm#invokestatic jc#get_this_path "main" (method_sig [] None);
			jm#return

		method private generate_fields =
			let field mtype cf = match cf.cf_kind with
				| Method (MethNormal | MethInline) ->
					List.iter (fun cf ->
						failsafe cf.cf_pos (fun () -> self#generate_method gctx jc c mtype cf);
						if cf.cf_name = "main" then self#generate_main;
					) (cf :: List.filter (fun cf -> Meta.has Meta.Overload cf.cf_meta) cf.cf_overloads)
				| _ ->
					if not c.cl_interface && is_physical_field cf then failsafe cf.cf_pos (fun () -> self#generate_field gctx jc c mtype cf)
			in
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
					let cf = mk_field "<clinit>" (tfun [] gctx.com.basic.tvoid) null_pos null_pos in
					cf.cf_kind <- Method MethNormal;
					let tf = {
						tf_args = [];
						tf_type = gctx.com.basic.tvoid;
						tf_expr = mk_block e;
					} in
					let e = mk (TFunction tf) cf.cf_type null_pos in
					cf.cf_expr <- Some e;
					field MStatic cf
			end

	method private generate_signature =
			let stl = match c.cl_params with
				| [] -> ""
				| params ->
					let stl = String.concat "" (List.map (fun (n,_) ->
						Printf.sprintf "%s:Ljava/lang/Object;" n
					) c.cl_params) in
					Printf.sprintf "<%s>" stl
			in
			let ssuper = match c.cl_super with
				| Some(c,tl) -> generate_method_signature true (jsignature_of_type (TInst(c,tl)))
				| None -> generate_method_signature true object_sig
			in
			let sinterfaces = String.concat "" (List.map (fun(c,tl) ->
				generate_method_signature true (jsignature_of_type (TInst(c,tl)))
			) c.cl_implements) in
			let s = Printf.sprintf "%s%s%s" stl ssuper sinterfaces in
			let offset = jc#get_pool#add_string s in
			jc#add_attribute (AttributeSignature offset)

	method generate_annotations =
		AnnotationHandler.generate_annotations (jc :> JvmBuilder.base_builder) c.cl_meta;
		jc#add_annotation (["haxe";"jvm";"annotation"],"ClassReflectionInformation") (["hasSuperClass",(ABool (c.cl_super <> None))])

	method generate =
		self#set_access_flags;
		self#generate_fields;
		self#set_interfaces;
		if not c.cl_interface then begin
			self#generate_empty_ctor;
			self#generate_implicit_ctors;
			self#handle_relation_type_params;
		end;
		self#generate_signature;
		if not (Meta.has Meta.NativeGen c.cl_meta) then
			generate_dynamic_access gctx jc (List.map (fun cf -> cf.cf_name,jsignature_of_type cf.cf_type,cf.cf_kind) c.cl_ordered_fields) false;
		self#generate_annotations;
		jc#add_attribute (AttributeSourceFile (jc#get_pool#add_string c.cl_pos.pfile));
		let jc = jc#export_class gctx.default_export_config in
		write_class gctx.jar c.cl_path jc
end

let generate_class gctx c =
	let conv = new tclass_to_jvm gctx c in
	conv#generate

let generate_enum gctx en =
	let jc_enum = new JvmClass.builder en.e_path haxe_enum_path in
	jc_enum#add_access_flag 0x1; (* public *)
	jc_enum#add_access_flag 0x400; (* abstract *)
	jc_enum#add_access_flag 0x4000; (* enum *)
	begin
		let jsig = haxe_enum_sig (object_path_sig en.e_path) in
		let s = generate_signature true jsig in
		let offset = jc_enum#get_pool#add_string s in
		jc_enum#add_attribute (AttributeSignature offset)
	end;
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
		jm_ctor#get_code#return_void;
	end;
	let inits = DynArray.create () in
	let names = List.map (fun name ->
		let ef = PMap.find name en.e_constrs in
		let args = match follow ef.ef_type with
			| TFun(tl,_) -> List.map (fun (n,_,t) -> n,jsignature_of_type t) tl
			| _ -> []
		in
		let jsigs = List.map snd args in
		(* Create class for constructor *)
		let jc_ctor = begin
			let jc_ctor = jc_enum#spawn_inner_class None jc_enum#get_this_path (Some ef.ef_name) in
			jc_ctor#add_access_flag 0x10; (* final *)
			jc_ctor#add_access_flag 0x4000; (* enum *)
			let jsig_method = method_sig jsigs None in
			let jm_ctor = jc_ctor#spawn_method "<init>" jsig_method [MPublic] in
			jm_ctor#load_this;
			jm_ctor#get_code#iconst (Int32.of_int ef.ef_index);
			jm_ctor#string ef.ef_name;
			jm_ctor#call_super_ctor ConstructInit jsig_enum_ctor;
			List.iter (fun (n,jsig) ->
				jm_ctor#add_argument_and_field n jsig
			) args;
			jm_ctor#get_code#return_void;
			jc_ctor#add_annotation (["haxe";"jvm";"annotation"],"EnumValueReflectionInformation") (["argumentNames",AArray (List.map (fun (name,_) -> AString name) args)]);
			if args <> [] then begin
				let jm_params = jc_ctor#spawn_method "_hx_getParameters" (method_sig [] (Some (array_sig object_sig))) [MPublic] in
				let fl = List.map (fun (n,jsig) ->
					(fun () ->
						jm_params#load_this;
						jm_params#getfield jc_ctor#get_this_path n jsig;
						jm_params#cast object_sig;
					)
				) args in
				jm_params#new_native_array object_sig fl;
				jm_params#return
			end;
			jc_ctor
		end in
		write_class gctx.jar jc_ctor#get_this_path (jc_ctor#export_class gctx.default_export_config);
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
				jm_static#get_code#return_value jc_enum#get_jsig;
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
			let handler = new texpr_to_jvm gctx jc_enum jm_clinit (gctx.com.basic.tvoid) in
			handler#texpr rvalue_any e;
			jm_clinit#putstatic jc_enum#get_this_path "__meta__" object_sig
		end;
		jm_clinit#get_code#return_void;
	end;
	AnnotationHandler.generate_annotations (jc_enum :> JvmBuilder.base_builder) en.e_meta;
	jc_enum#add_annotation (["haxe";"jvm";"annotation"],"EnumReflectionInformation") (["constructorNames",AArray names]);
	write_class gctx.jar en.e_path (jc_enum#export_class gctx.default_export_config)

let generate_abstract gctx a =
	let super_path = object_path in
	let jc = new JvmClass.builder a.a_path super_path in
	jc#add_access_flag 1; (* public *)
	let jc = jc#export_class gctx.default_export_config in
	write_class gctx.jar a.a_path jc

let debug_path path = match path with
	(* | ([],"Main") | (["haxe";"jvm"],_) -> true *)
	| (["haxe";"lang"],_) -> false (* Old Haxe/Java stuff that's weird *)
	| _ -> true

let is_extern_abstract a = match a.a_impl with
	| Some {cl_extern = true} -> true
	| _ -> false

let generate_module_type ctx mt =
	failsafe (t_infos mt).mt_pos (fun () ->
		match mt with
		| TClassDecl c when not c.cl_extern && debug_path c.cl_path -> generate_class ctx c
		| TEnumDecl en when not en.e_extern -> generate_enum ctx en
		| TAbstractDecl a when not (is_extern_abstract a) && Meta.has Meta.CoreType a.a_meta -> generate_abstract ctx a
		| _ -> ()
	)

module Preprocessor = struct

	let is_normal_anon an = match !(an.a_status) with
		| Closed | Const | Opened -> true
		| _ -> false

	let check_anon gctx e = match e.etype,follow e.etype with
		| TType(td,_),TAnon an when is_normal_anon an ->
			ignore(TAnonIdentifiaction.identify_as gctx td.t_path an.a_fields)
		| _ ->
			()

	let add_implicit_ctor gctx c c' cf =
		let jsig = jsignature_of_type cf.cf_type in
		try
			let sm = Hashtbl.find gctx.implicit_ctors c.cl_path in
			Hashtbl.replace gctx.implicit_ctors c.cl_path (PMap.add (c'.cl_path,jsig) (c',cf) sm);
		with Not_found ->
			Hashtbl.add gctx.implicit_ctors c.cl_path (PMap.add (c'.cl_path,jsig) (c',cf) PMap.empty)

	let make_native cf =
		cf.cf_meta <- (Meta.NativeGen,[],null_pos) :: cf.cf_meta

	let make_haxe cf =
		cf.cf_meta <- (Meta.HxGen,[],null_pos) :: cf.cf_meta

	let preprocess_constructor_expr gctx c cf e =
		let used_this = ref false in
		let this_before_super = ref false in
		let super_call_fields = DynArray.create () in
		let is_on_current_class cf = PMap.mem cf.cf_name c.cl_fields in
		let find_super_ctor el =
			let csup,map_type = match c.cl_super with
				| Some(c,tl) -> c,apply_params c.cl_params tl
				| _ -> assert false
			in
			match find_overload_rec' true map_type csup "new" el with
			| Some(c,cf) ->
				let rec loop csup =
					if c != csup then begin
						match csup.cl_super with
						| Some(c',_) ->
							add_implicit_ctor gctx csup c' cf;
							loop c'
						| None -> assert false
					end
				in
				loop csup;
				(c,cf)
			| None -> Error.error "Could not find overload constructor" e.epos
		in
		let rec promote_this_before_super c cf = match get_field_info gctx cf.cf_meta with
			| None -> jerror "Something went wrong"
			| Some info ->
				if not info.has_this_before_super then begin
					make_haxe cf;
					(* print_endline (Printf.sprintf "promoted this_before_super to %s.new : %s" (s_type_path c.cl_path) (s_type (print_context()) cf.cf_type)); *)
					info.has_this_before_super <- true;
					List.iter (fun (c,cf) -> promote_this_before_super c cf) info.super_call_fields
				end
		in
		let rec loop e =
			check_anon gctx e;
			begin match e.eexpr with
			| TBinop(OpAssign,{eexpr = TField({eexpr = TConst TThis},FInstance(_,_,cf))},e2) when is_on_current_class cf->
				(* Assigning this.field = value is fine if field is declared on our current class *)
				loop e2;
			| TConst TThis ->
				used_this := true
			| TCall({eexpr = TConst TSuper},el) ->
				List.iter loop el;
				if !used_this then begin
					this_before_super := true;
					make_haxe cf;
					(* print_endline (Printf.sprintf "inferred this_before_super on %s.new : %s" (s_type_path c.cl_path) (s_type (print_context()) cf.cf_type)); *)
				end;
				let c,cf = find_super_ctor el in
				if !this_before_super then promote_this_before_super c cf;
				DynArray.add super_call_fields (c,cf);
			| _ ->
				Type.iter loop e
			end;
		in
		loop e;
		{
			has_this_before_super = !this_before_super;
			super_call_fields = DynArray.to_list super_call_fields;
		}

	let preprocess_expr gctx e =
		let rec loop e =
			check_anon gctx e;
			Type.iter loop e
		in
		loop e

	let check_overrides c = match c.cl_overrides with
		| []->
			()
		| fields ->
			let csup,map_type = match c.cl_super with
				| Some(c,tl) -> c,apply_params c.cl_params tl
				| None -> assert false
			in
			let fix_covariant_return cf =
				let tl = match follow cf.cf_type with
					| TFun(tl,_) -> tl
					| _ -> assert false
				in
				match find_overload_rec' false map_type csup cf.cf_name (List.map (fun (_,_,t) -> Texpr.Builder.make_null t null_pos) tl) with
				| Some(_,cf') ->
					let tr = match follow cf'.cf_type with
						| TFun(_,tr) -> tr
						| _ -> assert false
					in
					cf.cf_type <- TFun(tl,tr);
					cf.cf_expr <- begin match cf.cf_expr with
						| Some ({eexpr = TFunction tf} as e) ->
							Some {e with eexpr = TFunction {tf with tf_type = tr}}
						| e ->
							e
					end;
				| None ->
					()
					(* TODO: this should never happen if we get the unification right *)
					(* Error.error "Could not find overload" cf.cf_pos *)
			in
			List.iter (fun cf ->
				fix_covariant_return cf;
				List.iter fix_covariant_return cf.cf_overloads
			) fields

	let rec get_constructor c =
		match c.cl_constructor, c.cl_super with
		| Some cf, _ -> c,cf
		| None, None -> raise Not_found
		| None, Some (csup,cparams) -> get_constructor csup

	let preprocess_class gctx c =
		let field cf = match cf.cf_expr with
			| None ->
				()
			| Some e ->
				preprocess_expr gctx e
		in
		let has_dynamic_instance_method = ref false in
		let has_field_init = ref false in
		let field mtype cf =
			List.iter field (cf :: cf.cf_overloads);
			match mtype with
			| MConstructor ->
				()
			| MInstance ->
				begin match cf.cf_kind with
					| Method MethDynamic -> has_dynamic_instance_method := true
					| Var _ when cf.cf_expr <> None && not !has_field_init && c.cl_constructor = None && c.cl_super = None ->
						has_field_init := true;
						add_implicit_ctor gctx c c (mk_field "new" (tfun [] gctx.com.basic.tvoid) null_pos null_pos)
					| _ -> ()
				end;
			| MStatic ->
				()
		in
		check_overrides c;
		List.iter (field MStatic) c.cl_ordered_statics;
		List.iter (field MInstance) c.cl_ordered_fields;
		match c.cl_constructor with
		| None ->
			begin try
				let csup,cf = get_constructor c in
				List.iter (fun cf -> add_implicit_ctor gctx c csup cf) (cf :: cf.cf_overloads)
			with Not_found ->
				()
			end;
		| Some cf ->
			let field cf =
				if !has_dynamic_instance_method then make_haxe cf;
				begin match cf.cf_expr with
				| None ->
					()
				| Some e ->
					let info = preprocess_constructor_expr gctx c cf e in
					let index = DynArray.length gctx.field_infos in
					DynArray.add gctx.field_infos info;
					cf.cf_meta <- (Meta.Custom ":jvm.fieldInfo",[(EConst (Int (string_of_int index)),null_pos)],null_pos) :: cf.cf_meta;
					if not (Meta.has Meta.HxGen cf.cf_meta) then begin
						let rec loop next c =
							if c.cl_extern then make_native cf
							else match c.cl_constructor with
								| Some cf' when Meta.has Meta.HxGen cf'.cf_meta -> make_haxe cf
								| Some cf' when Meta.has Meta.NativeGen cf'.cf_meta -> make_native cf
								| _ -> next c
						in
						let rec up c = match c.cl_super with
							| None -> ()
							| Some(c,_) -> loop up c
						in
						let rec down c = List.iter (fun c -> loop down c) c.cl_descendants in
						loop up c;
						loop down c
					end;
				end
			in
			List.iter field (cf :: cf.cf_overloads)

	let make_root path =
		["haxe";"root"],snd path

	let preprocess gctx =
		List.iter (fun mt ->
			match mt with
			| TClassDecl c ->
				if fst c.cl_path = [] then c.cl_path <- make_root c.cl_path;
				if debug_path c.cl_path && not c.cl_interface then preprocess_class gctx c
			| TEnumDecl en ->
				if fst en.e_path = [] then en.e_path <- make_root en.e_path;
			| _ -> ()
		) gctx.com.types
end

let file_name_and_extension file =
	match List.rev (ExtString.String.nsplit file "/") with
	| e1 :: _ -> e1
	| _ -> assert false

let generate com =
	mkdir_from_path com.file;
	let jar_name,manifest_suffix = match com.main_class with
		| Some path ->
			let pack = match fst path with
				| [] -> ["haxe";"root"]
				| pack -> pack
			in
			let name = snd path in
			name,"\nMain-Class: " ^ (s_type_path (pack,name))
		| None -> "jar",""
	in
	let jar_name = if com.debug then jar_name ^ "-Debug" else jar_name in
	let jar_dir = add_trailing_slash com.file in
	let jar_path = Printf.sprintf "%s%s.jar" jar_dir jar_name in
	let gctx = {
		com = com;
		jar = Zip.open_out jar_path;
		t_exception = TInst(resolve_class com (["java";"lang"],"Exception"),[]);
		t_throwable = TInst(resolve_class com (["java";"lang"],"Throwable"),[]);
		anon_lut = Hashtbl.create 0;
		anon_path_lut = Hashtbl.create 0;
		anon_num = 0;
		implicit_ctors = Hashtbl.create 0;
		field_infos = DynArray.create();
		current_field_info = None;
		default_export_config = {
			export_debug = com.debug;
		}
	} in
	Std.finally (Timer.timer ["generate";"java";"preprocess"]) Preprocessor.preprocess gctx;
	let class_paths = ExtList.List.filter_map (fun java_lib ->
		if java_lib#has_flag NativeLibraries.FlagIsStd then None
		else begin
			let dir = Printf.sprintf "%slib/" jar_dir in
			Path.mkdir_from_path dir;
			let name = file_name_and_extension java_lib#get_file_path in
			let ch_in = open_in_bin java_lib#get_file_path in
			let ch_out = open_out_bin (Printf.sprintf "%s%s" dir name) in
			let b = IO.read_all (IO.input_channel ch_in) in
			output_string ch_out b;
			close_in ch_in;
			close_out ch_out;
			Some (Printf.sprintf "lib/%s" name)
		end
	) com.native_libs.java_libs in
	let manifest_content =
		"Manifest-Version: 1.0\n" ^
		(match class_paths with [] -> "" | _ -> "Class-Path: " ^ (String.concat " " class_paths ^ "\n")) ^
		"Created-By: Haxe (Haxe Foundation)" ^
		manifest_suffix ^
		"\n\n"
	in
	Zip.add_entry manifest_content gctx.jar "META-INF/MANIFEST.MF";
	Hashtbl.iter (fun name v ->
		let filename = Codegen.escape_res_name name true in
		Zip.add_entry v gctx.jar filename;
	) com.resources;
	List.iter (generate_module_type gctx) com.types;
	Hashtbl.iter (fun fields path ->
		let jc = new JvmClass.builder path haxe_dynamic_object_path in
		jc#add_access_flag 0x1;
		begin
			let jm_ctor = jc#spawn_method "<init>" (method_sig (List.map snd fields) None) [MPublic] in
			jm_ctor#load_this;
			jm_ctor#get_code#aconst_null haxe_empty_constructor_sig;
			jm_ctor#call_super_ctor ConstructInit (method_sig [haxe_empty_constructor_sig] None);
			List.iter (fun (name,jsig) ->
				jm_ctor#add_argument_and_field name jsig;
			) fields;
			jm_ctor#get_code#return_void;
		end;
		begin
			let string_map_path = (["haxe";"ds"],"StringMap") in
			let string_map_sig = object_path_sig string_map_path in
			let jm_fields = jc#spawn_method "_hx_getKnownFields" (method_sig [] (Some string_map_sig)) [MProtected] in
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
			jm_fields#get_code#return_value string_map_sig
		end;
		generate_dynamic_access gctx jc (List.map (fun (name,jsig) -> name,jsig,Var {v_write = AccNormal;v_read = AccNormal}) fields) true;
		write_class gctx.jar path (jc#export_class gctx.default_export_config)
	) gctx.anon_lut;
	Zip.close_out gctx.jar