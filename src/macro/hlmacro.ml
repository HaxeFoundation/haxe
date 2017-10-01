(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
open Type
open Ast
open Hlcode
open Hlinterp
open MacroApi

type value = Hlinterp.value

type context = {
	com : Common.context; (* macro one *)
	mutable gen : Genhl.context option;
	interp : Hlinterp.context;
	types : (Type.path,int) Hashtbl.t;
	cached_protos : (obj_type, (virtual_proto * vfield array)) Hashtbl.t;
	cached_enums : (enum_type, ttype) Hashtbl.t;
	mutable curapi : value MacroApi.compiler_api;
	mutable has_error : bool;
}

exception Error of string * Globals.pos list

let debug = true (* TODO !!! set to false for speed++ ! *)

let get_ctx_ref = ref (fun() -> assert false)
let get_ctx() : context = (!get_ctx_ref)()
let macro_lib = Hashtbl.create 0
let interp() = (get_ctx()).interp

let setup get_api =
	let api = get_api (fun() -> (get_ctx()).curapi.get_com()) (fun() -> (get_ctx()).curapi) in
	List.iter (fun (n,v) -> Hashtbl.replace macro_lib n (match v with VClosure (FNativeFun (_,v,_),None) -> v | _ -> assert false)) api;
	Globals.macro_platform := Globals.Hl

let select ctx =
	get_ctx_ref := (fun() -> ctx)

let error_handler ctx v stack =
	let make_pos st =
		let file, line = Hlinterp.make_stack ctx.interp st in
		let low = line land 0xFFFFF in
		{
			Globals.pfile = file;
			Globals.pmin = low;
			Globals.pmax = low + (line lsr 20);
		}
	in
	(*let rec loop o =
		if o == ctx.error_proto then true else match o.oproto with None -> false | Some p -> loop p
	in
	(match v with
	| VObject o when loop o ->
		(match get_field o (hash "message"), get_field o (hash "pos") with
		| VObject msg, VAbstract (APos pos) ->
			(match get_field msg h_s with
			| VString msg -> raise (Error.Error (Error.Custom msg,pos))
			| _ -> ());
		| _ -> ());
	| _ -> ());*)
	raise (Error (Hlinterp.vstr ctx.interp v Hlcode.HDyn,List.map make_pos stack))

let create com api _ =
	let ctx = {
		com = com;
		gen = None;
		interp = Hlinterp.create debug;
		curapi = api;
		types = Hashtbl.create 0;
		has_error = false;
		cached_protos = Hashtbl.create 0;
		cached_enums = Hashtbl.create 0;
	} in
	select ctx;
	Hlinterp.set_error_handler ctx.interp (error_handler ctx);
	Hlinterp.set_macro_api ctx.interp (fun name -> try Some (Hashtbl.find macro_lib name) with Not_found -> None);
	ctx

let init ctx =
	if ctx.gen = None then ctx.gen <- Some (Genhl.create_context ctx.com true false)

let set_error ctx e =
	ctx.has_error <- e

let add_types ctx types ready =
	let types = List.filter (fun t -> match t with
		| TAbstractDecl a when not (Meta.has Meta.CoreType a.a_meta) ->
			(* A @:native on an abstract causes the implementation class and the abstract
			   to have the same path. Let's skip all abstracts so this doesn't matter. *)
			false
		| _ ->
			let path = Type.t_path t in
			if Hashtbl.mem ctx.types path then false else begin
				Hashtbl.add ctx.types path (Type.t_infos t).mt_module.m_id;
				true;
			end
	) types in
	List.iter ready types;
	if ctx.gen = None then init ctx;
	match ctx.gen with
	| None -> assert false
	| Some gen ->
		Genhl.add_types gen types;
		if debug then Genhl.check gen;
		let code = Genhl.build_code gen types None in
		if debug then begin
			try
				Hlinterp.check code true
			with Failure _ | Common.Abort _ as exn ->
				let ch = open_out_bin "hlcode.txt" in
				Hlcode.dump (fun s -> output_string ch (s ^ "\n")) code;
				close_out ch;
				raise exn
		end;
		Hlinterp.add_code ctx.interp code

let do_reuse ctx api =
	ctx.curapi <- api

let can_reuse ctx types =
	let has_old_version t =
		let inf = Type.t_infos t in
		try
			Hashtbl.find ctx.types inf.mt_path <> inf.mt_module.m_id
		with Not_found ->
			false
	in
	not (List.exists has_old_version types)

let catch_errors ctx ?(final=(fun() -> ())) f =
	let prev = get_ctx() in (* switch context in case we have an older one, see #5676 *)
	select ctx;
	try
		let v = f() in
		final();
		select prev;
		Some v
	with Error _ as e ->
		final();
		select prev;
		raise e
	|  Abort ->
		final();
		select prev;
		None

let call_path ctx cpath (f:string) args api =
	if ctx.has_error then
		None
	else let old = ctx.curapi in
	ctx.curapi <- api;
	let gid = Genhl.resolve_class_global (match ctx.gen with None -> assert false | Some ctx -> ctx) (String.concat "." cpath) in
	let gval = ctx.interp.Hlinterp.t_globals.(gid) in
	let fval = Hlinterp.dyn_get_field ctx.interp gval f Hlcode.HDyn in
	match fval with
	| Hlinterp.VClosure (f,None) ->
		catch_errors ~final:(fun() -> ctx.curapi <- old) ctx (fun() -> call_wrap ctx.interp f args)
	| _ ->
		prerr_endline (Hlinterp.vstr_d ctx.interp gval);
		assert false

let vnull = VNull
let vbool b = VBool b
let vint i = VInt (Int32.of_int i)
let vint32 i = VInt i
let vfloat f = VFloat f

let vfun0 f =
	let callb args = match args with [] -> f() | _ -> assert false in
	VClosure(FNativeFun ("fun0",callb,HVoid), None)

let vfun1 f =
	let callb args = match args with [a] -> f a | _ -> assert false in
	VClosure(FNativeFun ("fun1",callb,HVoid), None)

let vfun2 f =
	let callb args = match args with [a;b] -> f a b | _ -> assert false in
	VClosure(FNativeFun ("fun2",callb,HVoid), None)

let vfun3 f =
	let callb args = match args with [a;b;c] -> f a b c | _ -> assert false in
	VClosure(FNativeFun ("fun3",callb,HVoid), None)

let vfun4 f =
	let callb args = match args with [a;b;c;d] -> f a b c d | _ -> assert false in
	VClosure(FNativeFun ("fun4",callb,HVoid), None)

let vfun5 f =
	let callb args = match args with [a;b;c;d;e] -> f a b c d e | _ -> assert false in
	VClosure(FNativeFun ("fun5",callb,HVoid), None)

let exc_string msg =
	Hlinterp.throw_msg (interp()) msg

let compiler_error msg pos =
	assert false (* TODO : raise haxe.macro.Error(msg,pos) *)

let eval_expr ctx (e:texpr) =
	assert false

let eval_delayed _ _ =
	assert false (* macro - in - macro *)

let prepare_callback f n =
	assert false

let encode_pos p = VAbstract (APos p)

let decode_pos = function
	| VAbstract (APos p) -> p
	| _ -> raise Invalid_expr

let last_enum_type = ref IExpr

let encode_enum t pos tag pl =
	last_enum_type := t;
	assert false (* todo : list enum prototypes *)
	(*
	match pos with
	| None -> VEnum (tag,Array.of_list pl)
	| Some p -> VEnum (tag,Array.of_list (List.rev (encode_pos p :: List.rev pl)))
	*)

let decode_enum = function
	| VEnum (_,tag,arr) -> tag, Array.to_list arr
	| _ -> raise Invalid_expr

let decode_enum_with_pos = function
	| VEnum (_,tag,arr) when Array.length arr > 0 && (match arr.(Array.length arr - 1) with VAbstract (APos _) -> true | _ -> false) ->
		let rec loop i =
			if i = Array.length arr - 1 then [] else arr.(i) :: loop (i + 1)
		in
		(tag, loop 0), decode_pos arr.(Array.length arr - 1)
	| e ->
		decode_enum e, Globals.null_pos


let encode_tdecl t = VAbstract (ATDecl t)
let encode_unsafe o = VAbstract (AUnsafe o)

let decode_unsafe = function
	| VAbstract (AUnsafe v) -> v
	| _ -> raise Invalid_expr

let decode_tdecl = function
	| VAbstract (ATDecl t) -> t
	| _ -> raise Invalid_expr

let decode_int = function
	| VInt i -> Int32.to_int i
	| _ -> raise Invalid_expr

let decode_i32 = function
	| VInt i -> i
	| _ -> raise Invalid_expr

let decode_bool = function
	| VBool b -> b
	| _ -> raise Invalid_expr

let field o n =
	match o with
	| VDynObj _ | VVirtual _ | VObj _ -> Hlinterp.dyn_get_field (interp()) o n HDyn
	| _ -> raise Invalid_expr

let decode_string = function
	| VObj { ofields = [|VBytes s;VInt _|] } -> Hlinterp.hl_to_caml s
	| _ -> raise Invalid_expr

let decode_array = function
	| VObj { ofields = [|VInt len;VArray (arr,_)|] } ->
		let len = Int32.to_int len in
		let rec loop i =
			if i = len then [] else arr.(i) :: loop (i+1)
		in
		loop 0
	| v ->
		raise Invalid_expr

let encode_lazytype f t = VAbstract (ALazyType (f,t))

let decode_lazytype = function
	| VAbstract (ALazyType (t,_)) -> t
	| _ -> raise Invalid_expr

let encode_obj t fields =
	match t with
	| OMetaAccess ->
		let h = Hashtbl.create 0 in
		let rec loop i = function
			| [] -> ()
			| (n,_) :: l ->
				Hashtbl.add h n i;
				loop (i + 1) l
		in
		loop 0 fields;
		let values = Array.of_list (List.map snd fields) in
		VDynObj {
			dfields = h;
			dvalues = values;
			dtypes = Array.make (Array.length values) HDyn;
			dvirtuals = [];
		}
	| _ ->
	let ctx = get_ctx() in
	let to_str (name,f) =
		match f with
		| None -> name
		| Some f -> name ^ "." ^ f
	in
	let vp, idx = try
		Hashtbl.find ctx.cached_protos t
	with Not_found ->
		let name, field = proto_name t in
		let gen = (match ctx.gen with None -> assert false | Some gen -> gen) in
		let vt = (try
			let t = Hashtbl.find gen.Genhl.macro_typedefs name in
			(match t, field with
			| _, None -> t
			| HVirtual v, Some f ->
				let idx = (try PMap.find f v.vindex with Not_found -> failwith (name ^ " has no field definition " ^ f)) in
				let _,_, t = v.vfields.(idx) in
				(match t with
				| HVirtual _ -> t
				| _ -> failwith ("Unexpected type " ^ tstr t ^ " for definition " ^ to_str (name,field)))
			| _ ->
				assert false
			)
		with Not_found -> try
			let t = PMap.find (["haxe";"macro"],name) gen.Genhl.cached_types in
			(match t, field with
			| HEnum e, Some f ->
				let rec loop i =
					if i = Array.length e.efields then raise Not_found;
					let n, _, tl = e.efields.(i) in
					if n = f then
						tl.(0)
					else
						loop (i + 1)
				in
				loop 0
			| _ ->
				failwith ("Unexpected type " ^ tstr t ^ " for definition " ^ to_str (name,field)))
		with Not_found ->
			failwith ("Macro definition missing " ^ to_str (name,field))
		) in
		match vt with
		| HVirtual vp ->
			let vindexes = Array.map (fun (n,_,_) ->
				let rec loop i = function
					| [] -> VFNone
					| (n2,_) :: _ when n = n2 -> VFIndex i
					| _ :: l -> loop (i + 1) l
				in
				loop 0 fields
			) vp.vfields in
			Hashtbl.replace ctx.cached_protos t (vp, vindexes);
			vp, vindexes
		| _ ->
			failwith (to_str (name,field) ^ " returned invalid type " ^ tstr vt)
	in
	if debug then begin
		let farr = Array.of_list fields in
		Array.iteri (fun i idx ->
			let name, _ ,_ = vp.vfields.(i) in
			match idx with
			| VFNone ->
				if List.mem_assoc name fields then failwith ("Field " ^ name ^ " is present in "  ^ to_str (proto_name t))
			| VFIndex i when i >= Array.length farr ->
				failwith ("Missing field " ^ name ^ " of "  ^ to_str (proto_name t))
			| VFIndex i when fst farr.(i) <> name ->
				failwith ("Field " ^ name ^ " of "  ^ to_str (proto_name t) ^ " is wrongly mapped on " ^ fst farr.(i))
			| _ ->
				()
		) idx;
		List.iter (fun (n,_) ->
			if n <> "name_pos" && not (PMap.mem n vp.vindex) then failwith ("Field " ^ n ^ " has data but is not part of type " ^ to_str (proto_name t));
		) fields;
	end;
	VVirtual {
		vtype = vp;
		vindexes = idx;
		vtable = Array.map snd (Array.of_list fields);
		vvalue = VNull;
	}

let encode_inst path fields =
	let ctx = get_ctx() in
	let t = (match ctx.gen with None -> assert false | Some gen -> try Genhl.resolve_type gen path with Not_found -> assert false) in
	match t with
	| HObj o ->
		let proto, _, _ = Hlinterp.get_proto ctx.interp o in
		VObj { oproto = proto; ofields = fields }
	| _ ->
		assert false

let encode_string s =
	encode_inst ([],"String") [|VBytes (caml_to_hl s);VInt (Int32.of_int (String.length s))|]

let encode_array vl =
	let arr = Array.of_list (List.map (fun v ->
		match v with
		| VNull | VObj _ | VVirtual _ -> v
		| VEnum _ ->
			let ctx = get_ctx() in
			let et = !last_enum_type in
			let t = try
				Hashtbl.find ctx.cached_enums et
			with Not_found ->
				let name = enum_name et in
				let t = (match ctx.gen with
				| None -> assert false
				| Some gen -> try PMap.find (["haxe";"macro"],name) gen.Genhl.cached_types with Not_found -> failwith ("Missing enum type " ^ name)
				) in
				Hashtbl.replace ctx.cached_enums et t;
				t
			in
			VDyn (v,t)
		| _ -> failwith "Invalid array value"
	) vl) in
	encode_inst (["hl";"types"],"ArrayObj") [|VInt (Int32.of_int (Array.length arr));VArray (arr,HDyn)|]

let encode_bytes s =
	encode_inst (["haxe";"io"],"Bytes") [|VInt (Int32.of_int (String.length s)); VBytes s|]

let encode_string_map convert pmap =
	let h = Hashtbl.create 0 in
	PMap.iter (fun k v -> Hashtbl.add h k (convert v)) pmap;
	encode_inst (["haxe";"ds"],"StringMap") [|VAbstract (AHashBytes h)|]

let decode_bytes = function
	| VObj { ofields = [|VInt _;VBytes s|] } -> s
	| _ -> raise Invalid_expr

let encode_ref v convert tostr =
	let h = Hashtbl.create 0 in
	Hashtbl.add h "get" 0;
	Hashtbl.add h "__string" 1;
	Hashtbl.add h "toString" 2;
	Hashtbl.add h "$" 3;
	VDynObj {
		dfields = h;
		dvalues = [|
			vfun0 (fun() -> convert v);
			vfun0 (fun() -> VBytes (caml_to_hl (tostr())));
			vfun0 (fun() -> encode_string (tostr()));
			VAbstract (AUnsafe (Obj.repr v))
		|];
		dtypes = [|
			HFun ([],HDyn);
			HFun ([],HBytes);
			HFun ([],HDyn);
			HDyn;
		|];
		dvirtuals = [];
	}

let decode_ref v : 'a =
	match field v "$" with
	| VAbstract (AUnsafe t) -> Obj.obj t
	| _ -> raise Invalid_expr

let value_string v =
	Hlinterp.vstr (get_ctx()).interp v HDyn

let value_signature v =
	failwith "signature() not supported in HL macros"

let value_to_expr v p =
	let ctx = get_ctx() in
	let error v = failwith ("Unsupported value " ^ vstr ctx.interp v Hlcode.HDyn) in
	(*
	let h_enum = hash "__enum__" and h_et = hash "__et__" and h_ct = hash "__ct__" in
	let h_tag = hash "tag" and h_args = hash "args" in
	let h_length = hash "length" in
	let make_path t =
		let rec loop = function
			| [] -> assert false
			| [name] -> (Ast.EConst (Ast.Ident name),p)
			| name :: l -> (Ast.EField (loop l,name),p)
		in
		let t = t_infos t in
		loop (List.rev (if t.mt_module.m_path = t.mt_path then fst t.mt_path @ [snd t.mt_path] else fst t.mt_module.m_path @ [snd t.mt_module.m_path;snd t.mt_path]))
	in*)
	let rec loop = function
		| VNull -> (Ast.EConst (Ast.Ident "null"),p)
		| VBool b -> (Ast.EConst (Ast.Ident (if b then "true" else "false")),p)
		| VInt i -> (Ast.EConst (Ast.Int (Int32.to_string i)),p)
		| VFloat f ->
			let std = (Ast.EConst (Ast.Ident "std"), p) in
			let math = (Ast.EField (std, "Math"), p) in
			if (f = infinity) then
				(Ast.EField (math, "POSITIVE_INFINITY"), p)
			else if (f = neg_infinity) then
				(Ast.EField (math, "NEGATIVE_INFINITY"), p)
			else if (f <> f) then
				(Ast.EField (math, "NaN"), p)
			else
				(Ast.EConst (Ast.Float (Common.float_repres f)), p)
		| VAbstract (APos p) ->
			(Ast.EObjectDecl (
				(("fileName",Globals.null_pos,NoQuotes) , (Ast.EConst (Ast.String (p.Globals.pfile,Double)) , p)) ::
				(("lineNumber",Globals.null_pos,NoQuotes) , (Ast.EConst (Ast.Int (string_of_int (Lexer.get_error_line p))),p)) ::
				(("className",Globals.null_pos,NoQuotes) , (Ast.EConst (Ast.String ("",Double)),p)) ::
				[]
			), p)
		| VObj { oproto = { pclass = { pname = "String" } }; ofields = [|VBytes content;VInt _|] } ->
			(Ast.EConst (Ast.String (hl_to_caml content,Double)),p)
		| v ->
			error v
		(*
		| VObject o as v ->
			match o.oproto with
			| None ->
				(match get_field_opt o h_ct with
				| Some (VAbstract (ATDecl t)) ->
					make_path t
				| _ ->
					let fields = List.fold_left (fun acc (fid,v) -> ((field_name ctx fid,Globals.null_pos), loop v) :: acc) [] (Array.to_list o.ofields) in
					(Ast.EObjectDecl fields, p))
			| Some proto ->
				match get_field_opt proto h_enum, get_field_opt o h_a, get_field_opt o h_s, get_field_opt o h_length with
				| _, Some (VArray a), _, Some (VInt len) ->
					(Ast.EArrayDecl (List.map loop (Array.to_list (Array.sub a 0 len))),p)
				| Some (VObject en), _, _, _ ->
					(match get_field en h_et, get_field o h_tag with
					| VAbstract (ATDecl t), VString tag ->
						let e = (Ast.EField (make_path t,tag),p) in
						(match get_field_opt o h_args with
						| Some (VArray args) ->
							let args = List.map loop (Array.to_list args) in
							(Ast.ECall (e,args),p)
						| _ -> e)
					| _ ->
						error v)
				| _ ->
					error v
			*)
	in
	loop v
