(*
 * Copyright (C)2005-2015 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)
open Ast
open Type
open Common

type reg = int
type global = int
type 'a index = int
type functable

type ttype =
	| HVoid
	| HI8
	| HI16
	| HI32
	| HF32
	| HF64
	| HBool
	| HBytes
	| HDyn of ttype option
	| HFun of ttype list * ttype
	| HObj of class_proto
	| HArray
	| HType
	| HRef of ttype
	| HVirtual of virtual_proto
	| HDynObj
	| HAbstract of string * string index
	| HEnum of enum_proto

and class_proto = {
	pname : string;
	pid : int;
	mutable psuper : class_proto option;
	mutable pvirtuals : int array;
	mutable pproto : field_proto array;
	mutable pfields : (string * string index * ttype) array;
	mutable pindex : (string, int * ttype) PMap.t;
	mutable pfunctions : (string, int) PMap.t;
}

and enum_proto = {
	ename : string;
	eid : int;
	mutable efields : (string * string index * ttype array) array;
}

and field_proto = {
	fname : string;
	fid : int;
	fmethod : functable index;
	fvirtual : int option;
}

and virtual_proto = {
	mutable vfields : (string * string index * ttype) array;
	mutable vindex : (string, int) PMap.t;
}

type unused = int
type field

type opcode =
	| OMov of reg * reg
	| OInt of reg * int index
	| OFloat of reg * float index
	| OBool of reg * bool
	| OString of reg * string index
	| ONull of reg
	| OAdd of reg * reg * reg
	| OSub of reg * reg * reg
	| OMul of reg * reg * reg
	| OSDiv of reg * reg * reg
	| OUDiv of reg * reg * reg
	| OSMod of reg * reg * reg
	| OUMod of reg * reg * reg
	| OShl of reg * reg * reg
	| OSShr of reg * reg * reg
	| OUShr of reg * reg * reg
	| OAnd of reg * reg * reg
	| OOr of reg * reg * reg
	| OXor of reg * reg * reg
	| ONeg of reg * reg
	| ONot of reg * reg
	| OIncr of reg
	| ODecr of reg
	| OCall0 of reg * functable index
	| OCall1 of reg * functable index * reg
	| OCall2 of reg * functable index * reg * reg
	| OCall3 of reg * functable index * reg * reg * reg
	| OCall4 of reg * functable index * reg * reg * reg * reg
	| OCallN of reg * functable index * reg list
	| OCallMethod of reg * field index * reg list
	| OCallThis of reg * field index * reg list
	| OCallClosure of reg * reg * reg list
	| OGetFunction of reg * functable index (* closure *)
	| OClosure of reg * functable index * reg (* closure *)
	| OGetGlobal of reg * global
	| OSetGlobal of global * reg
	| OEq of reg * reg * reg
	| ONotEq of reg * reg * reg
	| OSLt of reg * reg * reg
	| OSGte of reg * reg * reg
	| OULt of reg * reg * reg
	| OUGte of reg * reg * reg
	| ORet of reg
	| OJTrue of reg * int
	| OJFalse of reg * int
	| OJNull of reg * int
	| OJNotNull of reg * int
	| OJSLt of reg * reg * int
	| OJSGte of reg * reg * int
	| OJULt of reg * reg * int
	| OJUGte of reg * reg * int
	| OJEq of reg * reg * int
	| OJNeq of reg * reg * int
	| OJAlways of int
	| OToDyn of reg * reg
	| OToFloat of reg * reg
	| OToInt of reg * reg
	| OLabel of unused
	| ONew of reg
	| OField of reg * reg * field index
	| OMethod of reg * reg * field index (* closure *)
	| OSetField of reg * field index * reg
	| OGetThis of reg * field index
	| OSetThis of field index * reg
	| OThrow of reg
	| OGetI8 of reg * reg * reg
	| OGetI32 of reg * reg * reg
	| OGetF32 of reg * reg * reg
	| OGetF64 of reg * reg * reg
	| OGetArray of reg * reg * reg
	| OSetI8 of reg * reg * reg
	| OSetI32 of reg * reg * reg
	| OSetF32 of reg * reg * reg
	| OSetF64 of reg * reg * reg
	| OSetArray of reg * reg * reg
	| OSafeCast of reg * reg
	| OUnsafeCast of reg * reg
	| OArraySize of reg * reg
	| OError of string index
	| OType of reg * ttype
	| ORef of reg * reg
	| OUnref of reg * reg
	| OSetref of reg * reg
	| OToVirtual of reg * reg
	| OUnVirtual of reg * reg
	| ODynGet of reg * reg * string index
	| ODynSet of reg * string index * reg
	| OMakeEnum of reg * field index * reg list
	| OEnumAlloc of reg * field index
	| OEnumIndex of reg * reg
	| OEnumField of reg * reg * field index * int
	| OSetEnumField of reg * int * reg
	| OSwitch of reg * int array
	| ONullCheck of reg
	| OTrap of reg * int
	| OEndTrap of unused
	| ODump of reg

type fundecl = {
	findex : functable index;
	ftype : ttype;
	regs : ttype array;
	code : opcode array;
	debug : (int * int) array;
}

type code = {
	version : int;
	entrypoint : global;
	strings : string array;
	ints : int32 array;
	floats : float array;
	(* types : ttype array // only in bytecode, rebuilt on save() *)
	globals : ttype array;
	natives : (string index * string index * ttype * functable index) array;
	functions : fundecl array;
	debugfiles : string array;
}

(* compiler *)

type ('a,'b) lookup = {
	arr : 'b DynArray.t;
	mutable map : ('a, int) PMap.t;
}

(* not mutable, might be be shared *)
type method_capture = {
	c_map : (int, int) PMap.t;
	c_vars : tvar array;
	c_type : ttype;
}

type method_context = {
	mregs : (int, ttype) lookup;
	mops : opcode DynArray.t;
	mret : ttype;
	mdebug : (int * int) DynArray.t;
	mutable mcaptured : method_capture;
	mutable mcontinues : (int -> unit) list;
	mutable mbreaks : (int -> unit) list;
	mutable mtrys : int;
	mutable mcaptreg : int;
	mutable mcurpos : (int * int);
}

type array_impl = {
	aobj : tclass;
	ai32 : tclass;
	af64 : tclass;
}

type context = {
	com : Common.context;
	cglobals : (string, ttype) lookup;
	cstrings : (string, string) lookup;
	cfloats : (float, float) lookup;
	cints : (int32, int32) lookup;
	cnatives : (string, (string index * string index * ttype * functable index)) lookup;
	cfids : (string * path, unit) lookup;
	cfunctions : fundecl DynArray.t;
	overrides : (string * path, bool) Hashtbl.t;
	defined_funs : (int,unit) Hashtbl.t;
	mutable cached_types : (path, ttype) PMap.t;
	mutable m : method_context;
	mutable anons_cache : (tanon * ttype) list;
	mutable method_wrappers : ((ttype * ttype), int) PMap.t;
	array_impl : array_impl;
	cdebug_files : (string, string) lookup;
}

(* --- *)

type access =
	| ANone
	| AGlobal of global
	| ALocal of reg
	| AStaticFun of fundecl index
	| AInstanceFun of texpr * fundecl index
	| AInstanceProto of texpr * field index
	| AInstanceField of texpr * field index
	| AArray of texpr * texpr
	| AVirtualMethod of texpr * field index
	| ADynamic of texpr * string index
	| AEnum of field index
	| ACaptured of field index

let list_iteri f l =
	let p = ref 0 in
	List.iter (fun v -> f !p v; incr p) l

let field_type f =
	match f with
	| FInstance (_,_,f) | FStatic (_,f) | FAnon f | FClosure (_,f) -> f.cf_type
	| FDynamic _ -> t_dynamic
	| FEnum (_,f) -> f.ef_type

let rec tstr ?(stack=[]) ?(detailed=false) t =
	match t with
	| HVoid -> "void"
	| HI8 -> "i8"
	| HI16 -> "i16"
	| HI32 -> "i32"
	| HF32 -> "f32"
	| HF64 -> "f64"
	| HBool -> "bool"
	| HBytes -> "bytes"
	| HDyn None -> "dyn"
	| HDyn (Some t) -> "dyn(" ^ tstr t ^ ")"
	| HFun (args,ret) -> "(" ^ String.concat "," (List.map (tstr ~detailed) args) ^ "):" ^ tstr ~detailed ret
	| HObj o when not detailed -> "#" ^ o.pname
	| HObj o ->
		let fields = "{" ^ String.concat "," (List.map (fun(s,_,t) -> s ^ " : " ^ tstr ~detailed:false t) (Array.to_list o.pfields)) ^ "}" in
		let proto = "{"  ^ String.concat "," (List.map (fun p -> (match p.fvirtual with None -> "" | Some _ -> "virtual ") ^ p.fname ^ "@" ^  string_of_int p.fmethod) (Array.to_list o.pproto)) ^ "}" in
		"#" ^ o.pname ^ "[" ^ (match o.psuper with None -> "" | Some p -> ">" ^ p.pname ^ " ") ^ "fields=" ^ fields ^ " proto=" ^ proto ^ "]"
	| HArray ->
		"array"
	| HType ->
		"type"
	| HRef t ->
		"ref(" ^ tstr t ^ ")"
	| HVirtual v when List.memq v stack ->
		"..."
	| HVirtual v ->
		"virtual(" ^ String.concat "," (List.map (fun (f,_,t) -> f ^":"^tstr ~stack:(v::stack) t) (Array.to_list v.vfields)) ^ ")"
	| HDynObj ->
		"dynobj"
	| HAbstract (s,_) ->
		"abstract(" ^ s ^ ")"
	| HEnum e when e.eid = 0 ->
		let _,_,fl = e.efields.(0) in
		"enum(" ^ String.concat "," (List.map tstr (Array.to_list fl)) ^ ")"
	| HEnum e ->
		"enum(" ^ e.ename ^ ")"

let rec tsame t1 t2 =
	if t1 == t2 then true else
	match t1, t2 with
	| HFun (args1,ret1), HFun (args2,ret2) when List.length args1 = List.length args2 -> List.for_all2 tsame args1 args2 && tsame ret2 ret1
	| HObj p1, HObj p2 -> p1 == p2
	| HEnum e1, HEnum e2 -> e1 == e2
	| HAbstract (_,a1), HAbstract (_,a2) -> a1 == a2
	| HVirtual v1, HVirtual v2 ->
		if v1 == v2 then true else
		if Array.length v1.vfields <> Array.length v2.vfields then false else
		let rec loop i =
			if i = Array.length v1.vfields then true else
			let _, i1, t1 = v1.vfields.(i) in
			let _, i2, t2 = v2.vfields.(i) in
			if i1 = i2 && tsame t1 t2 then loop (i + 1) else false
		in
		loop 0
	| HDyn None, HDyn None -> true
	| HDyn (Some t1), HDyn (Some t2) -> tsame t1 t2
	| HRef t1, HRef t2 -> tsame t1 t2
	| _ -> false

let rec safe_cast t1 t2 =
	if t1 == t2 then true else
	match t1, t2 with
	| (HDyn _ | HObj _ | HFun _ | HArray | HDynObj), HDyn None -> true
	| HVirtual v1, HVirtual v2 when Array.length v2.vfields < Array.length v1.vfields ->
		let rec loop i =
			if i = Array.length v2.vfields then true else
			let n1, _, t1 = v1.vfields.(i) in
			let n2, _, t2 = v2.vfields.(i) in
			if n1 = n2 && tsame t1 t2 then loop (i + 1) else false
		in
		loop 0
	| HObj p1, HObj p2 ->
		(* allow subtyping *)
		let rec loop p =
			p.pname = p2.pname || (match p.psuper with None -> false | Some p -> loop p)
		in
		loop p1
	| HFun (args1,t1), HFun (args2,t2) when List.length args1 = List.length args2 ->
		List.for_all2 (fun t1 t2 -> safe_cast t2 t1 || (t2 = HDyn None && safe_cast t1 t2)) args1 args2 && safe_cast t1 t2
	| _ ->
		tsame t1 t2

let to_utf8 str =
	try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code ->
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UChar.of_char c)) str;
			UTF8.Buf.contents b

let iteri f l =
	let p = ref (-1) in
	List.iter (fun v -> incr p; f !p v) l

let new_lookup() =
	{
		arr = DynArray.create();
		map = PMap.empty;
	}

let null_proto =
	{
		pname = "";
		pid = 0;
		psuper = None;
		pvirtuals = [||];
		pproto = [||];
		pfields = [||];
		pindex = PMap.empty;
		pfunctions = PMap.empty;
	}

let null_capture =
	{
		c_vars = [||];
		c_map = PMap.empty;
		c_type = HVoid;
	}

let lookup l v fb =
	try
		PMap.find v l.map
	with Not_found ->
		let id = DynArray.length l.arr in
		DynArray.add l.arr (Obj.magic 0);
		l.map <- PMap.add v id l.map;
		DynArray.set l.arr id (fb());
		id

let lookup_alloc l v =
	let id = DynArray.length l.arr in
	DynArray.add l.arr v;
	id

let method_context t captured =
	{
		mregs = new_lookup();
		mops = DynArray.create();
		mret = t;
		mbreaks = [];
		mcontinues = [];
		mcaptured = captured;
		mtrys = 0;
		mcaptreg = 0;
		mdebug = DynArray.create();
		mcurpos = (0,0);
	}

let field_name c f =
	s_type_path c.cl_path ^ ":" ^ f.cf_name

let efield_name e f =
	s_type_path e.e_path ^ ":" ^ f.ef_name

let global_type ctx g =
	DynArray.get ctx.cglobals.arr g

let is_overriden ctx c f =
	Hashtbl.mem ctx.overrides (f.cf_name,c.cl_path)

let alloc_float ctx f =
	lookup ctx.cfloats f (fun() -> f)

let alloc_i32 ctx i =
	lookup ctx.cints i (fun() -> i)

let alloc_string ctx s =
	lookup ctx.cstrings s (fun() -> s)

let member_fun c t =
	match follow t with
	| TFun (args, ret) -> TFun (("this",false,TInst(c,[])) :: args, ret)
	| _ -> assert false

let rec unsigned t =
	match follow t with
	| TAbstract ({ a_path = ["hl";"types"],("UI32"|"UI16"|"UI8") },_) -> true
	| TAbstract (a,pl) -> unsigned (Abstract.get_underlying_type a pl)
	| _ -> false

let set_curpos ctx p =
	let get_relative_path() =
		match Common.defined ctx.com Common.Define.AbsolutePath with
		| true -> if (Filename.is_relative p.pfile)
			then Filename.concat (Sys.getcwd()) p.pfile
			else p.pfile
		| false -> try
			(* lookup relative path *)
			let len = String.length p.pfile in
			let base = List.find (fun path ->
				let l = String.length path in
				len > l && String.sub p.pfile 0 l = path
			) ctx.com.Common.class_path in
			let l = String.length base in
			String.sub p.pfile l (len - l)
		with Not_found ->
			p.pfile
	in
	ctx.m.mcurpos <- (lookup ctx.cdebug_files p.pfile get_relative_path,Lexer.get_error_line p)

let rec to_type ctx t =
	match t with
	| TMono r ->
		(match !r with
		| None -> HDyn None
		| Some t -> to_type ctx t)
	| TType (t,tl) ->
		(match t.t_path with
		| [], "Null" ->
			(match to_type ctx (apply_params t.t_params tl t.t_type) with
			| HI8 | HI16 | HI32 | HF32 | HF64 | HBool as t -> HDyn (Some t)
			| t -> t)
		| _ ->
			to_type ctx (apply_params t.t_params tl t.t_type))
	| TLazy f ->
		to_type ctx (!f())
	| TFun (args, ret) ->
		HFun (List.map (fun (_,_,t) -> to_type ctx t) args, to_type ctx ret)
	| TAnon a when (match !(a.a_status) with Statics _ | EnumStatics _  -> true | _ -> false) ->
		HType
	| TAnon a ->
		(try
			(* can't use physical comparison in PMap since addresses might change in GC compact,
				maybe add an uid to tanon if too slow ? *)
			List.assq a ctx.anons_cache
		with Not_found ->
			let vp = {
				vfields = [||];
				vindex = PMap.empty;
			} in
			let t = HVirtual vp in
			ctx.anons_cache <- (a,t) :: ctx.anons_cache;
			let fields = PMap.fold (fun cf acc ->
				match cf.cf_kind with
				| Var { v_read = AccNo } | Var { v_write = AccNo } ->
					(*
						if there's read-only/write-only fields, it will allow variance, so let's
						handle the field access as fully Dynamic
					*)
					acc
				| Var _ when has_meta Meta.Optional cf.cf_meta ->
					(*
						if it's optional it might not be present, handle the field access as fully Dynamic
					*)
					acc
				| _ ->
					(cf.cf_name,alloc_string ctx cf.cf_name,to_type ctx cf.cf_type) :: acc
			) a.a_fields [] in
			let fields = List.sort (fun (n1,_,_) (n2,_,_) -> compare n1 n2) fields in
			vp.vfields <- Array.of_list fields;
			Array.iteri (fun i (n,_,_) -> vp.vindex <- PMap.add n i vp.vindex) vp.vfields;
			t
		)
	| TDynamic _ ->
		HDyn None
	| TEnum (e,_) ->
		enum_type ctx e
	| TInst ({ cl_path = ["hl";"types"],"NativeAbstract" },[TInst({ cl_kind = KExpr (EConst (String name),_) },_)]) ->
		HAbstract (name, alloc_string ctx name)
	| TInst (c,pl) ->
		(match c.cl_kind with
		| KTypeParameter _ -> HDyn None
		| _ -> class_type ctx c pl)
	| TAbstract (a,pl) ->
		if Meta.has Meta.CoreType a.a_meta then
			(match a.a_path with
			| [], "Void" -> HVoid
			| [], "Int" -> HI32
			| [], "Float" -> HF64
			| [], "Single" -> HF32
			| [], "Bool" -> HBool
			| [], "Class" | [], "Enum" -> HType
			| [], "EnumValue" -> HDyn None
			| ["hl";"types"], "Ref" -> HRef (to_type ctx (List.hd pl))
			| ["hl";"types"], "Bytes" -> HBytes
			| ["hl";"types"], "NativeArray" -> HArray
			| _ -> failwith ("Unknown core type " ^ s_type_path a.a_path))
		else
			to_type ctx (Abstract.get_underlying_type a pl)

and array_type ctx t =
	let et = to_type ctx t in
	if safe_cast et (HDyn None) then et else HDyn None

and resolve_class ctx c pl =
	let not_supported() =
		failwith ("Extern type not supported : " ^ s_type (print_context()) (TInst (c,pl)))
	in
	match c.cl_path, pl with
	| ([],"Array"), [t] ->
		(match to_type ctx t with
		| HI32 ->
			ctx.array_impl.ai32
		| HF64 ->
			ctx.array_impl.af64
		| _ ->
			ctx.array_impl.aobj)
	| _, _ when c.cl_extern ->
		not_supported()
	| _ ->
		c

and class_type ctx c pl =
	let c = if c.cl_extern then resolve_class ctx c pl else c in
	try
		PMap.find c.cl_path ctx.cached_types
	with Not_found when c.cl_interface ->
		let vp = {
			vfields = [||];
			vindex = PMap.empty;
		} in
		let t = HVirtual vp in
		ctx.cached_types <- PMap.add c.cl_path t ctx.cached_types;
		let fields = PMap.fold (fun cf acc -> (cf.cf_name,alloc_string ctx cf.cf_name,to_type ctx cf.cf_type) :: acc) c.cl_fields [] in
		vp.vfields <- Array.of_list fields;
		Array.iteri (fun i (n,_,_) -> vp.vindex <- PMap.add n i vp.vindex) vp.vfields;
		t
	| Not_found ->
		let pname = s_type_path c.cl_path in
		let p = {
			pname = pname;
			pid = alloc_string ctx pname;
			psuper = None;
			pproto = [||];
			pfields = [||];
			pindex = PMap.empty;
			pvirtuals = [||];
			pfunctions = PMap.empty;
		} in
		let t = HObj p in
		ctx.cached_types <- PMap.add c.cl_path t ctx.cached_types;
		let start_field, virtuals = (match c.cl_super with
			| None -> 0, [||]
			| Some (c,pl) ->
				match class_type ctx c pl with
				| HObj psup ->
					p.psuper <- Some psup;
					p.pindex <- psup.pindex;
					p.pfunctions <- psup.pfunctions;
					Array.length p.pfields, p.pvirtuals
				| _ -> assert false
		) in
		let fa = DynArray.create() and pa = DynArray.create() and virtuals = DynArray.of_array virtuals in
		List.iter (fun f ->
			if is_extern_field f then () else
			match f.cf_kind with
			| Var _ | Method MethDynamic ->
				let t = to_type ctx f.cf_type in
				p.pindex <- PMap.add f.cf_name (DynArray.length fa + start_field, t) p.pindex;
				DynArray.add fa (f.cf_name, alloc_string ctx f.cf_name, t);
			| Method _ ->
				let g = alloc_fid ctx c f in
				p.pfunctions <- PMap.add f.cf_name g p.pfunctions;
				let virt = if List.memq f c.cl_overrides then
					Some (try fst (PMap.find f.cf_name p.pindex) with Not_found -> assert false)
				else if is_overriden ctx c f then begin
					let vid = DynArray.length virtuals in
					DynArray.add virtuals g;
					p.pindex <- PMap.add f.cf_name (vid,HVoid) p.pindex;
					Some vid
				end else
					None
				in
				DynArray.add pa { fname = f.cf_name; fid = alloc_string ctx f.cf_name; fmethod = g; fvirtual = virt; }
		) c.cl_ordered_fields;
		(try
			let cf = PMap.find "toString" c.cl_fields in
			if List.memq cf c.cl_overrides || PMap.mem "__string" c.cl_fields then raise Not_found;
			DynArray.add pa { fname = "__string"; fid = alloc_string ctx "__string"; fmethod = alloc_fun_path ctx c.cl_path "__string"; fvirtual = None; }
		with Not_found ->
			());
		p.pfields <- DynArray.to_array fa;
		p.pproto <- DynArray.to_array pa;
		p.pvirtuals <- DynArray.to_array virtuals;
		t

and enum_type ctx e =
	try
		PMap.find e.e_path ctx.cached_types
	with Not_found ->
		let ename = s_type_path e.e_path in
		let et = {
			ename = ename;
			eid = alloc_string ctx ename;
			efields = [||];
		} in
		let t = HEnum et in
		ctx.cached_types <- PMap.add e.e_path t ctx.cached_types;
		et.efields <- Array.of_list (List.map (fun f ->
			let f = PMap.find f e.e_constrs in
			let args = (match f.ef_type with
				| TFun (args,_) -> Array.of_list (List.map (fun (_,_,t) -> to_type ctx t) args)
				| _ -> [||]
			) in
			(f.ef_name, alloc_string ctx f.ef_name, args)
		) e.e_names);
		t

and alloc_fid ctx c f =
	match f.cf_kind with
	| Var _ | Method MethDynamic -> assert false
	| _ -> lookup ctx.cfids (f.cf_name, c.cl_path) (fun() -> ())

and alloc_eid ctx e f =
	lookup ctx.cfids (f.ef_name, e.e_path) (fun() -> ())

and alloc_fun_path ctx path name =
	lookup ctx.cfids (name, path) (fun() -> ())

and alloc_function_name ctx f =
	lookup ctx.cfids (f, ([],"")) (fun() -> ())

let alloc_std ctx name args ret =
	let lib = "std" in
	let nid = lookup ctx.cnatives (name ^ "@" ^ lib) (fun() ->
		let fid = lookup ctx.cfids (name, ([],"std")) (fun() -> ()) in
		Hashtbl.add ctx.defined_funs fid ();
		(alloc_string ctx lib, alloc_string ctx name,HFun (args,ret),fid)
	) in
	let _,_,_,fid = DynArray.get ctx.cnatives.arr nid in
	fid

let is_int ctx t =
	match to_type ctx t with
	| HI8 | HI16 | HI32 -> true
	| _ -> false

let is_float ctx t =
	match to_type ctx t with
	| HF32 | HF64 -> true
	| _ -> false

let alloc_global ctx name t =
	lookup ctx.cglobals name (fun() -> to_type ctx t)

let alloc_reg ctx v =
	lookup ctx.m.mregs v.v_id (fun() -> to_type ctx v.v_type)

let alloc_tmp ctx t =
	let rid = DynArray.length ctx.m.mregs.arr in
	DynArray.add ctx.m.mregs.arr t;
	rid

let op ctx o =
	DynArray.add ctx.m.mdebug ctx.m.mcurpos;
	DynArray.add ctx.m.mops o

let current_pos ctx =
	DynArray.length ctx.m.mops

let jump ctx f =
	let pos = current_pos ctx in
	op ctx (OJAlways (-1)); (* loop *)
	(fun() -> DynArray.set ctx.m.mops pos (f (current_pos ctx - pos - 1)))

let jump_back ctx =
	let pos = current_pos ctx in
	op ctx (OLabel 0);
	(fun() -> op ctx (OJAlways (pos - current_pos ctx - 1)))

let rtype ctx r =
	DynArray.get ctx.m.mregs.arr r

let reg_int ctx v =
	let r = alloc_tmp ctx HI32 in
	op ctx (OInt (r,alloc_i32 ctx (Int32.of_int v)));
	r

let common_type ctx e1 e2 for_eq p =
	let t1 = to_type ctx e1.etype in
	let t2 = to_type ctx e2.etype in
	let rec loop t1 t2 =
		if t1 == t2 then t1 else
		match t1, t2 with
		| HI8, (HI16 | HI32 | HF32 | HF64) -> t2
		| HI16, (HI32 | HF32 | HF64) -> t2
		| HI32, HF32 -> t2 (* possible loss of precision *)
		| (HI32 | HF32), HF64 -> t2
		| (HI8|HI16|HI32|HF32|HF64), (HI8|HI16|HI32|HF32|HF64) -> t1
		| (HI8|HI16|HI32|HF32|HF64), (HDyn (Some t2)) -> loop t1 t2
		| (HDyn (Some t1)), (HI8|HI16|HI32|HF32|HF64) -> loop t1 t2
		| (HDyn None), (HI8|HI16|HI32|HF32|HF64) -> HF64
		| (HI8|HI16|HI32|HF32|HF64), (HDyn None) -> HF64
		| HDyn None, _ -> HDyn None
		| _, HDyn None -> HDyn None
		| _ when for_eq && safe_cast t1 t2 -> t2
		| _ when for_eq && safe_cast t2 t1 -> t1
		| _ ->
			error ("Don't know how to compare " ^ tstr t1 ^ " and " ^ tstr t2) p
	in
	loop t1 t2

let captured_index ctx v =
	if not v.v_capture then None else try Some (PMap.find v.v_id ctx.m.mcaptured.c_map) with Not_found -> None

let before_return ctx =
	let rec loop i =
		if i > 0 then begin
			op ctx (OEndTrap 0);
			loop (i - 1)
		end
	in
	loop ctx.m.mtrys

let rec eval_to ctx e (t:ttype) =
	let r = eval_expr ctx e in
	cast_to ctx r t e.epos

and cast_to ctx (r:reg) (t:ttype) p =
	let rt = rtype ctx r in
	let invalid() =
		error ("Don't know how to cast " ^ tstr rt ^ " to " ^ tstr t) p
	in
	if safe_cast rt t then r else
	match rt, t with
	| _, HVoid ->
		alloc_tmp ctx HVoid
	| HVirtual _, HDyn None ->
		let tmp = alloc_tmp ctx (HDyn None) in
		op ctx (OUnVirtual (tmp,r));
		tmp
	| HVirtual _, HVirtual _ ->
		let tmp = alloc_tmp ctx (HDyn None) in
		op ctx (OUnVirtual (tmp,r));
		cast_to ctx tmp t p
	| (HI8 | HI16 | HI32), (HF32 | HF64) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToFloat (tmp, r));
		tmp
	| (HI8 | HI16 | HI32), HObj { pname = "String" } ->
		let out = alloc_tmp ctx t in
		let len = alloc_tmp ctx HI32 in
		let lref = alloc_tmp ctx (HRef HI32) in
		let bytes = alloc_tmp ctx HBytes in
		op ctx (ORef (lref,len));
		op ctx (OCall2 (bytes,alloc_std ctx "itos" [HI32;HRef HI32] HBytes,cast_to ctx r HI32 p,lref));
		op ctx (OCall3 (out,alloc_fun_path ctx ([],"String") "__alloc__",bytes,len,len));
		out
	| (HF32 | HF64), HObj { pname = "String" } ->
		let out = alloc_tmp ctx t in
		let len = alloc_tmp ctx HI32 in
		let lref = alloc_tmp ctx (HRef HI32) in
		let bytes = alloc_tmp ctx HBytes in
		op ctx (ORef (lref,len));
		op ctx (OCall2 (bytes,alloc_std ctx "ftos" [HF64;HRef HI32] HBytes,cast_to ctx r HF64 p,lref));
		op ctx (OCall3 (out,alloc_fun_path ctx ([],"String") "__alloc__",bytes,len,len));
		out
	| (HObj _ | HDynObj | HDyn None) , HVirtual _ ->
		let out = alloc_tmp ctx t in
		op ctx (OToVirtual (out,r));
		out
	| HDyn None, _ ->
		let out = alloc_tmp ctx t in
		op ctx (OSafeCast (out, r));
		out
	| HDyn (Some rt), _ when rt == t ->
		let out = alloc_tmp ctx t in
		op ctx (OSafeCast (out, r));
		out
	| _ , HDyn _ ->
		let tmp = alloc_tmp ctx (HDyn (Some rt)) in
		op ctx (OToDyn (tmp, r));
		tmp
	| HFun (args1,ret1), HFun (args2, ret2) when List.length args1 = List.length args2 ->
		let fid = gen_method_wrapper ctx rt t p in
		let fr = alloc_tmp ctx t in
		op ctx (OClosure (fr,fid,r));
		fr
	| _ ->
		invalid()

and unsafe_cast_to ctx (r:reg) (t:ttype) p =
	let rt = rtype ctx r in
	if safe_cast rt t then
		r
	else
	match rt with
	| HFun _ ->
		cast_to ctx r t p
	| _ ->
		if safe_cast (rtype ctx r) (HDyn None) && safe_cast t (HDyn None) then
			let r2 = alloc_tmp ctx t in
			op ctx (OUnsafeCast (r2,r));
			r2
		else
			cast_to ctx r t p

and object_access ctx eobj t f =
	match t with
	| HObj p ->
		(try
			let fid = fst (PMap.find f.cf_name p.pindex) in
			if f.cf_kind = Method MethNormal then
				AInstanceProto (eobj, fid)
			else
				AInstanceField (eobj, fid)
		with Not_found ->
			ADynamic (eobj, alloc_string ctx f.cf_name))
	| HVirtual v ->
		(try
			let fid = PMap.find f.cf_name v.vindex in
			if f.cf_kind = Method MethNormal then
				AVirtualMethod (eobj, fid)
			else
				AInstanceField (eobj, fid)
		with Not_found ->
			ADynamic (eobj, alloc_string ctx f.cf_name))
	| HDyn None ->
		ADynamic (eobj, alloc_string ctx f.cf_name)
	| _ ->
		error ("Unsupported field access " ^ tstr t) eobj.epos

and get_access ctx e =
	match e.eexpr with
	| TField (ethis, a) ->
		(match a, follow ethis.etype with
		| FStatic (c,({ cf_kind = Var _ | Method MethDynamic } as f)), _ ->
			AGlobal (alloc_global ctx (field_name c f) f.cf_type)
		| FStatic (c,({ cf_kind = Method _ } as f)), _ ->
			AStaticFun (alloc_fid ctx c f)
		| FClosure (Some (cdef,pl), ({ cf_kind = Method m } as f)), TInst (c,_)
		| FInstance (cdef,pl,({ cf_kind = Method m } as f)), TInst (c,_) when m <> MethDynamic && not (c.cl_interface || is_overriden ctx c f) ->
			AInstanceFun (ethis, alloc_fid ctx (resolve_class ctx cdef pl) f)
		| FInstance (cdef,pl,f), _ | FClosure (Some (cdef,pl), f), _ ->
			object_access ctx ethis (class_type ctx cdef pl) f
		| FClosure (None,_), _ ->
			assert false
		| FAnon f, _ ->
			object_access ctx ethis (to_type ctx ethis.etype) f
		| FDynamic name, _ ->
			ADynamic (ethis, alloc_string ctx name)
		| FEnum (_,ef), _ ->
			AEnum ef.ef_index
		)
	| TLocal v ->
		(match captured_index ctx v with
		| None -> ALocal (alloc_reg ctx v)
		| Some idx -> ACaptured idx)
	| TParenthesis e ->
		get_access ctx e
	| TArray (a,i) ->
		AArray (a,i)
	| _ ->
		ANone

and jump_expr ctx e jcond =
	match e.eexpr with
	| TParenthesis e ->
		jump_expr ctx e jcond
	| TUnop (Not,_,e) ->
		jump_expr ctx e (not jcond)
	| TBinop (OpEq | OpNotEq | OpGt | OpGte | OpLt | OpLte as jop, e1, e2) ->
		let t = common_type ctx e1 e2 (match jop with OpEq | OpNotEq -> true | _ -> false) e.epos in
		let r1 = eval_to ctx e1 t in
		let r2 = eval_to ctx e2 t in
		let unsigned = unsigned e1.etype && unsigned e2.etype in
		jump ctx (fun i ->
			let lt a b = if unsigned then OJULt (a,b,i) else OJSLt (a,b,i) in
			let gte a b = if unsigned then OJUGte (a,b,i) else OJSGte (a,b,i) in
			match jop with
			| OpEq -> if jcond then OJEq (r1,r2,i) else OJNeq (r1,r2,i)
			| OpNotEq -> if jcond then OJNeq (r1,r2,i) else OJEq (r1,r2,i)
			| OpGt -> if jcond then lt r2 r1 else gte r2 r1
			| OpGte -> if jcond then gte r1 r2 else lt r1 r2
			| OpLt -> if jcond then lt r1 r2 else gte r1 r2
			| OpLte -> if jcond then gte r2 r1 else lt r2 r1
			| _ -> assert false
		)
	| TBinop (OpBoolAnd, e1, e2) ->
		let j = jump_expr ctx e1 false in
		let j2 = jump_expr ctx e2 jcond in
		if jcond then j();
		(fun() -> if not jcond then j(); j2());
	| TBinop (OpBoolOr, e1, e2) ->
		let j = jump_expr ctx e1 true in
		let j2 = jump_expr ctx e2 jcond in
		if not jcond then j();
		(fun() -> if jcond then j(); j2());
	| _ ->
		let r = eval_expr ctx e in
		jump ctx (fun i -> if jcond then OJTrue (r,i) else OJFalse (r,i))

and eval_args ctx el t =
	List.map2 (fun e t -> eval_to ctx e t) el (match t with HFun (args,_) -> args | HDyn None -> List.map (fun _ -> HDyn None) el | _ -> assert false)

and eval_null_check ctx e =
	let r = eval_expr ctx e in
	(match e.eexpr with
	| TConst TThis -> ()
	| _ -> op ctx (ONullCheck r));
	r

and eval_expr ctx e =
	set_curpos ctx e.epos;
	match e.eexpr with
	| TConst c ->
		(match c with
		| TInt i ->
			let r = alloc_tmp ctx HI32 in
			op ctx (OInt (r,alloc_i32 ctx i));
			r
		| TFloat f ->
			let r = alloc_tmp ctx HF64 in
			op ctx (OFloat (r,alloc_float ctx (float_of_string f)));
			r
		| TBool b ->
			let r = alloc_tmp ctx HBool in
			op ctx (OBool (r,b));
			r
		| TString s ->
			let s = to_utf8 s in
			let r = alloc_tmp ctx HBytes in
			op ctx (OString (r,alloc_string ctx s));
			let size = reg_int ctx (String.length s) in
			let len = reg_int ctx (UTF8.length s) in
			let s = alloc_tmp ctx (to_type ctx e.etype) in
			op ctx (OCall3 (s,alloc_fun_path ctx ([],"String") "__alloc__",r,size,len));
			s
		| TThis ->
			0 (* first reg *)
		| _ ->
			let r = alloc_tmp ctx (to_type ctx e.etype) in
			op ctx (ONull r);
			r)
	| TVar (v,e) ->
		(match e with
		| None -> ()
		| Some e ->
			match captured_index ctx v with
			| None ->
				let r = alloc_reg ctx v in
				let ri = eval_to ctx e (rtype ctx r) in
				op ctx (OMov (r,ri))
			| Some idx ->
				let ri = eval_to ctx e (to_type ctx v.v_type) in
				op ctx (OSetEnumField (ctx.m.mcaptreg, idx, ri));
		);
		alloc_tmp ctx HVoid
	| TLocal v ->
		(match captured_index ctx v with
		| None -> alloc_reg ctx v
		| Some idx ->
			let r = alloc_tmp ctx (to_type ctx v.v_type) in
			op ctx (OEnumField (r, ctx.m.mcaptreg, 0, idx));
			r)
	| TReturn None ->
		before_return ctx;
		let r = alloc_tmp ctx HVoid in
		op ctx (ORet r);
		r
	| TReturn (Some e) ->
		before_return ctx;
		let r = eval_to ctx e ctx.m.mret in
		op ctx (ORet r);
		alloc_tmp ctx HVoid
	| TParenthesis e ->
		eval_expr ctx e
	| TBlock el ->
		let rec loop = function
			| [e] -> eval_expr ctx e
			| [] -> alloc_tmp ctx HVoid
			| e :: l ->
				ignore(eval_expr ctx e);
				loop l
		in
		loop el
	| TCall ({ eexpr = TField (_,FStatic({ cl_path = [],"Type" },{ cf_name = "enumIndex" })) },[e]) when (match to_type ctx e.etype with HEnum _ -> true | _ -> false) ->
		let r = alloc_tmp ctx HI32 in
		let re = eval_expr ctx e in
		op ctx (ONullCheck re);
		op ctx (OEnumIndex (r,re));
		r
	| TCall ({ eexpr = TConst TSuper } as s, el) ->
		(match follow s.etype with
		| TInst (csup,_) ->
			(match csup.cl_constructor with
			| None -> assert false
			| Some f ->
				let r = alloc_tmp ctx HVoid in
				let el = eval_args ctx el (to_type ctx f.cf_type) in
				op ctx (OCallN (r, alloc_fid ctx csup f, 0 :: el));
				r
			)
		| _ -> assert false);
	| TCall ({ eexpr = TLocal v }, el) when v.v_name.[0] = '$' ->
		let invalid() = error "Invalid native call" e.epos in
		(match v.v_name, el with
		| "$new", [{ eexpr = TTypeExpr (TClassDecl _) }] ->
			(match follow e.etype with
			| TInst (c,pl) ->
				let r = alloc_tmp ctx (class_type ctx c pl) in
				op ctx (ONew r);
				r
			| _ ->
				invalid())
		| "$int", [{ eexpr = TBinop (OpDiv, e1, e2) }] when is_int ctx e1.etype && is_int ctx e2.etype ->
			let tmp = alloc_tmp ctx HI32 in
			op ctx (if unsigned e1.etype && unsigned e2.etype then OUDiv (tmp, eval_to ctx e1 HI32, eval_to ctx e2 HI32) else OSDiv (tmp, eval_to ctx e1 HI32, eval_to ctx e2 HI32));
			tmp
		| "$int", [e] ->
			let tmp = alloc_tmp ctx HI32 in
			op ctx (OToInt (tmp, eval_expr ctx e));
			tmp
		| "$balloc", [e] ->
			let f = alloc_std ctx "balloc" [HI32] HBytes in
			let tmp = alloc_tmp ctx HBytes in
			op ctx (OCall1 (tmp, f, eval_to ctx e HI32));
			tmp
		| "$bblit", [b;dp;src;sp;len] ->
			let f = alloc_std ctx "bblit" [HBytes;HI32;HBytes;HI32;HI32] HVoid in
			let tmp = alloc_tmp ctx HVoid in
			op ctx (OCallN (tmp, f, [eval_to ctx b HBytes;eval_to ctx dp HI32;eval_to ctx src HBytes;eval_to ctx sp HI32; eval_to ctx len HI32]));
			tmp
		| "$bseti8", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HI32 in
			op ctx (OSetI8 (b, pos, r));
			r
		| "$bseti32", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HI32 in
			op ctx (OSetI32 (b, pos, r));
			r
		| "$bsetf32", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HF32 in
			op ctx (OSetF32 (b, pos, r));
			r
		| "$bsetf64", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HF64 in
			op ctx (OSetF64 (b, pos, r));
			r
		| "$bgeti8", [b;pos] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx HI32 in
			op ctx (OGetI8 (r, b, pos));
			r
		| "$bgeti32", [b;pos] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx HI32 in
			op ctx (OGetI32 (r, b, pos));
			r
		| "$bgetf32", [b;pos] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx HF32 in
			op ctx (OGetF32 (r, b, pos));
			r
		| "$bgetf64", [b;pos] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = alloc_tmp ctx HF64 in
			op ctx (OGetF64 (r, b, pos));
			r
		| "$asize", [e] ->
			let r = alloc_tmp ctx HI32 in
			op ctx (OArraySize (r, eval_to ctx e HArray));
			r
		| "$aalloc", [esize] ->
			let et = (match follow e.etype with TAbstract ({ a_path = ["hl";"types"],"NativeArray" },[t]) -> array_type ctx t | _ -> invalid()) in
			let a = alloc_tmp ctx HArray in
			let rt = alloc_tmp ctx HType in
			op ctx (OType (rt,et));
			let size = eval_to ctx esize HI32 in
			op ctx (OCall2 (a,alloc_std ctx "aalloc" [HType;HI32] HArray,rt,size));
			a
		| "$aget", [a; pos] ->
			let at = (match follow a.etype with TAbstract ({ a_path = ["hl";"types"],"NativeArray" },[t]) -> to_type ctx t | _ -> invalid()) in
			let arr = eval_to ctx a HArray in
			let pos = eval_to ctx pos HI32 in
			let r =
				if safe_cast at (HDyn None) then
					let r = alloc_tmp ctx at in
					op ctx (OGetArray (r, arr, pos));
					r
				else
					let tmp = alloc_tmp ctx (HDyn None) in
					op ctx (OGetArray (tmp,arr,pos));
					unsafe_cast_to ctx tmp at e.epos
			in
			cast_to ctx r (to_type ctx e.etype) e.epos
		| "$aset", [a; pos; value] ->
			let et = (match follow a.etype with TAbstract ({ a_path = ["hl";"types"],"NativeArray" },[t]) -> array_type ctx t | _ -> invalid()) in
			let arr = eval_to ctx a HArray in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx value et in
			op ctx (OSetArray (arr, pos, r));
			r
		| "$ref", [v] ->
			let r = alloc_tmp ctx (to_type ctx e.etype) in
			let rv = (match rtype ctx r with HRef t -> eval_to ctx v t | _ -> invalid()) in
			op ctx (ORef (r,rv));
			r
		| "$dump", [v] ->
			op ctx (ODump (eval_expr ctx v));
			alloc_tmp ctx HVoid
		| _ ->
			error ("Unknown native call " ^ v.v_name) e.epos)
	| TCall (ec,el) ->
		let real_type = (match ec.eexpr with
			| TField (_,f) -> field_type f
			| _ -> ec.etype
		) in
		let tfun = to_type ctx real_type in
		let el = eval_args ctx el tfun in
		let ret = alloc_tmp ctx (match tfun with HFun (_,r) -> r | _ -> HDyn None) in
		(match get_access ctx ec with
		| AStaticFun f ->
			(match el with
			| [] -> op ctx (OCall0 (ret, f))
			| [a] -> op ctx (OCall1 (ret, f, a))
			| [a;b] -> op ctx (OCall2 (ret, f, a, b))
			| [a;b;c] -> op ctx (OCall3 (ret, f, a, b, c))
			| [a;b;c;d] -> op ctx (OCall4 (ret, f, a, b, c, d))
			| _ -> op ctx (OCallN (ret, f, el)));
		| AInstanceFun (ethis, f) ->
			let el = eval_null_check ctx ethis :: el in
			(match el with
			| [a] -> op ctx (OCall1 (ret, f, a))
			| [a;b] -> op ctx (OCall2 (ret, f, a, b))
			| [a;b;c] -> op ctx (OCall3 (ret, f, a, b, c))
			| [a;b;c;d] -> op ctx (OCall4 (ret, f, a, b, c, d))
			| _ -> op ctx (OCallN (ret, f, el)));
		| AInstanceProto ({ eexpr = TConst TThis }, fid) ->
			op ctx (OCallThis (ret, fid, el))
		| AInstanceProto (ethis, fid) ->
			let el = eval_null_check ctx ethis :: el in
			op ctx (OCallMethod (ret, fid, el))
		| AEnum index ->
			op ctx (OMakeEnum (ret, index, el))
		| _ ->
			let r = eval_null_check ctx ec in
			op ctx (OCallClosure (ret, r, el)); (* if it's a value, it's a closure *)
		);
		unsafe_cast_to ctx ret (to_type ctx e.etype) e.epos
	| TField (ec,a) ->
		let r = alloc_tmp ctx (to_type ctx (field_type a)) in
		(match get_access ctx e with
		| AGlobal g ->
			op ctx (OGetGlobal (r,g));
		| AStaticFun f ->
			op ctx (OGetFunction (r,f));
		| AInstanceFun (ethis, f) ->
			op ctx (OClosure (r, f, eval_null_check ctx ethis))
		| AInstanceField (ethis,fid) ->
			let robj = eval_null_check ctx ethis in
			op ctx (match ethis.eexpr with TConst TThis -> OGetThis (r,fid) | _ -> OField (r,robj,fid));
		| AInstanceProto (ethis,fid) | AVirtualMethod (ethis, fid) ->
			let robj = eval_null_check ctx ethis in
			op ctx (OMethod (r,robj,fid));
		| ADynamic (ethis, f) ->
			let robj = eval_null_check ctx ethis in
			op ctx (ODynGet (r,robj,f))
		| AEnum index ->
			op ctx (OMakeEnum (r,index,[]))
		| ANone | ALocal _ | AArray _ | ACaptured _ ->
			error "Invalid access" e.epos);
		unsafe_cast_to ctx r (to_type ctx e.etype) e.epos
	| TObjectDecl o ->
		let r = alloc_tmp ctx HDynObj in
		op ctx (ONew r);
		let a = (match follow e.etype with TAnon a -> a | _ -> assert false) in
		List.iter (fun (s,v) ->
			let cf = (try PMap.find s a.a_fields with Not_found -> assert false) in
			let v = eval_to ctx v (to_type ctx cf.cf_type) in
			op ctx (ODynSet (r,alloc_string ctx s,v));
		) o;
		r
	| TNew (c,pl,el) ->
		let c = resolve_class ctx c pl in
		let r = alloc_tmp ctx (class_type ctx c pl) in
		op ctx (ONew r);
		(match c.cl_constructor with
		| None -> ()
		| Some { cf_expr = None } -> error (s_type_path c.cl_path ^ " does not have a constructor") e.epos
		| Some ({ cf_expr = Some { eexpr = TFunction({ tf_expr = { eexpr = TBlock([]) } }) } }) when el = [] -> ()
		| Some ({ cf_expr = Some cexpr } as constr) ->
			let rl = eval_args ctx el (to_type ctx cexpr.etype) in
			let ret = alloc_tmp ctx HVoid in
			let g = alloc_fid ctx c constr in
			op ctx (match rl with
			| [] -> OCall1 (ret,g,r)
			| [a] -> OCall2 (ret,g,r,a)
			| [a;b] -> OCall3 (ret,g,r,a,b)
			| [a;b;c] -> OCall4 (ret,g,r,a,b,c)
			| _ ->
				let rf = alloc_tmp ctx (global_type ctx g) in
				op ctx (OGetGlobal (rf,g));
				OCallN (ret,rf,r :: rl));
		);
		r
	| TIf (cond,eif,eelse) ->
		let t = to_type ctx e.etype in
		let out = alloc_tmp ctx t in
		let j = jump_expr ctx cond false in
		if t = HVoid then ignore(eval_expr ctx eif) else op ctx (OMov (out,eval_to ctx eif t));
		(match eelse with
		| None -> j()
		| Some e ->
			let jexit = jump ctx (fun i -> OJAlways i) in
			j();
			if t = HVoid then ignore(eval_expr ctx e) else op ctx (OMov (out,eval_to ctx e t));
			jexit());
		out
	| TBinop (bop, e1, e2) ->
		let gte r a b =
			if unsigned e1.etype && unsigned e2.etype then
				OUGte (r,a,b)
			else
				OSGte (r,a,b)
		in
		let lt r a b =
			if unsigned e1.etype && unsigned e2.etype then
				OULt (r,a,b)
			else
				OSLt (r,a,b)
		in
		let binop r a b =
			let rec loop bop =
				match bop with
				| OpLte -> op ctx (gte r b a)
				| OpGt -> op ctx (lt r b a)
				| OpGte -> op ctx (gte r a b)
				| OpLt -> op ctx (lt r a b)
				| OpEq -> op ctx (OEq (r,a,b))
				| OpNotEq -> op ctx (ONotEq (r,a,b))
				| OpAdd ->
					(match rtype ctx r with
					| HI8 | HI16 | HI32 | HF32 | HF64 ->
						op ctx (OAdd (r,a,b))
					| HObj { pname = "String" } ->
						op ctx (OCall2 (r,alloc_fun_path ctx ([],"String") "__add__",a,b))
					| HDyn None ->
						op ctx (OCall2 (r,alloc_fun_path ctx ([],"Std") "__add__",a,b))
					| t ->
						error ("Cannot add " ^ tstr t) e.epos)
				| OpSub | OpMult | OpMod | OpDiv ->
					(match rtype ctx r with
					| HI8 | HI16 | HI32 | HF32 | HF64 ->
						(match bop with
						| OpSub -> op ctx (OSub (r,a,b))
						| OpMult -> op ctx (OMul (r,a,b))
						| OpMod -> op ctx (if unsigned e1.etype && unsigned e2.etype then OUMod (r,a,b) else OSMod (r,a,b))
						| OpDiv -> op ctx (if unsigned e1.etype && unsigned e2.etype then OUDiv (r,a,b) else OSDiv (r,a,b))
						| _ -> assert false)
					| _ ->
						assert false)
				| OpShl | OpShr | OpUShr | OpAnd | OpOr | OpXor ->
					(match rtype ctx r with
					| HI8 | HI16 | HI32 ->
						(match bop with
						| OpShl -> op ctx (OShl (r,a,b))
						| OpShr -> op ctx (if unsigned e1.etype then OUShr (r,a,b) else OSShr (r,a,b))
						| OpUShr -> op ctx (OUShr (r,a,b))
						| OpAnd -> op ctx (OAnd (r,a,b))
						| OpOr -> op ctx (OOr (r,a,b))
						| OpXor -> op ctx (OXor (r,a,b))
						| _ -> ())
					| _ ->
						assert false)
				| OpAssignOp bop ->
					loop bop
				| _ ->
					assert false
			in
			loop bop
		in
		(match bop with
		| OpLte | OpGt | OpGte | OpLt ->
			let r = alloc_tmp ctx HBool in
			let t = common_type ctx e1 e2 false e.epos in
			let a = eval_to ctx e1 t in
			let b = eval_to ctx e2 t in
			binop r a b;
			r
		| OpEq | OpNotEq ->
			let r = alloc_tmp ctx HBool in
			let t = common_type ctx e1 e2 true e.epos in
			let a = eval_to ctx e1 t in
			let b = eval_to ctx e2 t in
			binop r a b;
			r
		| OpAdd | OpSub | OpMult | OpDiv | OpMod | OpShl | OpShr | OpUShr | OpAnd | OpOr | OpXor ->
			let t = to_type ctx e.etype in
			let r = alloc_tmp ctx t in
			let a = eval_to ctx e1 t in
			let b = eval_to ctx e2 t in
			binop r a b;
			r
		| OpAssign ->
			let value() = eval_to ctx e2 (to_type ctx e1.etype) in
			(match get_access ctx e1 with
			| AGlobal g ->
				let r = value() in
				op ctx (OSetGlobal (g,r));
				r
			| AInstanceField ({ eexpr = TConst TThis }, fid) ->
				let r = value() in
				op ctx (OSetThis (fid,r));
				r
			| AInstanceField (ethis, fid) ->
				let rthis = eval_null_check ctx ethis in
				let r = value() in
				op ctx (OSetField (rthis, fid, r));
				r
			| ALocal l ->
				let r = value() in
				op ctx (OMov (l, r));
				r
			| AArray (a,idx) ->
				let a = eval_null_check ctx a in
				let idx = eval_to ctx idx HI32 in
				let v = value() in
				(* bounds check against length *)
				let len = alloc_tmp ctx HI32 in
				op ctx (OField (len,a,1));
				let j = jump ctx (fun i -> OJULt (idx,len,i)) in
				op ctx (OCall2 (alloc_tmp ctx HVoid, alloc_fun_path ctx (["hl";"types"],"ArrayObj") "__expand", a, idx));
				j();
				let arr = alloc_tmp ctx HArray in
				op ctx (OField (arr,a,0));
				op ctx (OSetArray (arr,idx,v));
				v
			| ADynamic (ethis,f) ->
				let obj = eval_null_check ctx ethis in
				let r = eval_expr ctx e2 in
				op ctx (ODynSet (obj,f,r));
				r
			| ACaptured index ->
				let r = value() in
				op ctx (OSetEnumField (ctx.m.mcaptreg,index,r));
				r
			| AEnum _ | ANone | AInstanceFun _ | AInstanceProto _ | AStaticFun _ | AVirtualMethod _ ->
				assert false)
		| OpBoolOr ->
			let r = alloc_tmp ctx HBool in
			let j = jump_expr ctx e1 true in
			let j2 = jump_expr ctx e2 true in
			op ctx (OBool (r,false));
			let jend = jump ctx (fun b -> OJAlways b) in
			j();
			j2();
			op ctx (OBool (r,true));
			jend();
			r
		| OpBoolAnd ->
			let r = alloc_tmp ctx HBool in
			let j = jump_expr ctx e1 false in
			let j2 = jump_expr ctx e2 false in
			op ctx (OBool (r,true));
			let jend = jump ctx (fun b -> OJAlways b) in
			j();
			j2();
			op ctx (OBool (r,false));
			jend();
			r
		| OpAssignOp bop ->
			(match get_access ctx e1 with
			| ALocal l ->
				let r = eval_to ctx { e with eexpr = TBinop (bop,e1,e2) } (to_type ctx e1.etype) in
				op ctx (OMov (l, r));
				r
			| AInstanceField (eobj, findex) ->
				let robj = eval_null_check ctx eobj in
				let t = to_type ctx e1.etype in
				let r = alloc_tmp ctx t in
				op ctx (OField (r,robj,findex));
				let b = eval_to ctx e2 t in
				binop r r b;
				op ctx (OSetField (robj,findex,r));
				r
			| _ ->
				error ("TODO " ^ s_expr (s_type (print_context())) e) e.epos
			)
		| OpInterval | OpArrow ->
			assert false)
	| TUnop (Not,_,v) ->
		let tmp = alloc_tmp ctx HBool in
		let r = eval_to ctx v HBool in
		op ctx (ONot (tmp,r));
		tmp
	| TUnop (Neg,_,v) ->
		let t = to_type ctx e.etype in
		let tmp = alloc_tmp ctx t in
		let r = eval_to ctx v t in
		op ctx (ONeg (tmp,r));
		tmp
	| TUnop (NegBits,_,v) ->
		let t = to_type ctx e.etype in
		let tmp = alloc_tmp ctx t in
		let r = eval_to ctx v t in
		let mask = (match t with
			| HI8 -> 0xFFl
			| HI16 -> 0xFFFFl
			| HI32 -> 0xFFFFFFFFl
			| _ -> assert false
		) in
		let r2 = alloc_tmp ctx t in
		op ctx (OInt (r2,alloc_i32 ctx mask));
		op ctx (OXor (tmp,r,r2));
		tmp
	| TUnop (Increment|Decrement as uop,fix,v) ->
		let unop r =
			match rtype ctx r with
			| HI8 | HI16 | HI32 ->
				if uop = Increment then op ctx (OIncr r) else op ctx (ODecr r)
			| HF32 | HF64 as t ->
				let tmp = alloc_tmp ctx t in
				op ctx (OFloat (tmp,alloc_float ctx 1.));
				if uop = Increment then op ctx (OAdd (r,r,tmp)) else op ctx (OSub (r,r,tmp))
			| _ ->
				assert false
		in
		(match get_access ctx v, fix with
		| ALocal r, Prefix ->
			unop r;
			r
		| ALocal r, Postfix ->
			let r2 = alloc_tmp ctx (rtype ctx r) in
			op ctx (OMov (r2,r));
			unop r;
			r2
		| AInstanceField (eobj,f), Prefix ->
			let robj = eval_expr ctx eobj in
			let r = alloc_tmp ctx (to_type ctx e.etype) in
			op ctx (OField (r,robj,f));
			unop r;
			op ctx (OSetField (robj,f,r));
			r
		| AInstanceField (eobj,f), Postfix ->
			let robj = eval_expr ctx eobj in
			let r = alloc_tmp ctx (to_type ctx e.etype) in
			op ctx (OField (r,robj,f));
			let r2 = alloc_tmp ctx (rtype ctx r) in
			op ctx (OMov (r2,r));
			unop r;
			op ctx (OSetField (robj,f,r));
			r2
		| AGlobal g, Prefix ->
			let r = alloc_tmp ctx (to_type ctx e.etype) in
			op ctx (OGetGlobal (r,g));
			unop r;
			op ctx (OSetGlobal (g,r));
			r
		| AGlobal g, Postfix ->
			let r = alloc_tmp ctx (to_type ctx e.etype) in
			let r2 = alloc_tmp ctx (rtype ctx r) in
			op ctx (OGetGlobal (r,g));
			op ctx (OMov (r2,r));
			unop r;
			op ctx (OSetGlobal (g,r));
			r2
		| _ ->
			error ("TODO " ^ s_expr (s_type (print_context())) e) e.epos
		);
	| TFunction f ->
		let fid = alloc_function_name ctx ("function#" ^ string_of_int (DynArray.length ctx.cfids.arr)) in
		let capt = make_fun ctx fid f None (Some ctx.m.mcaptured) in
		let r = alloc_tmp ctx (to_type ctx e.etype) in
		if capt == ctx.m.mcaptured then
			op ctx (OClosure (r, fid, ctx.m.mcaptreg))
		else if Array.length capt.c_vars > 0 then
			let env = alloc_tmp ctx capt.c_type in
			op ctx (OEnumAlloc (env,0));
			Array.iteri (fun i v ->
				let r = (match captured_index ctx v with
				| None -> alloc_reg ctx v
				| Some idx ->
					let r = alloc_tmp ctx (to_type ctx v.v_type) in
					op ctx (OEnumField (r,ctx.m.mcaptreg,0,idx));
					r
				) in
				op ctx (OSetEnumField (env,i,r));
			) capt.c_vars;
			op ctx (OClosure (r, fid, env))
		else
			op ctx (OGetFunction (r, fid));
		r
	| TThrow v ->
		op ctx (OThrow (eval_to ctx v (HDyn None)));
		alloc_tmp ctx HVoid (* not initialized *)
	| TWhile (cond,eloop,NormalWhile) ->
		let oldb = ctx.m.mbreaks and oldc = ctx.m.mcontinues in
		ctx.m.mbreaks <- [];
		ctx.m.mcontinues <- [];
		let continue_pos = current_pos ctx in
		let ret = jump_back ctx in
		let j = jump_expr ctx cond false in
		ignore(eval_expr ctx eloop);
		ret();
		j();
		List.iter (fun f -> f (current_pos ctx)) ctx.m.mbreaks;
		List.iter (fun f -> f continue_pos) ctx.m.mcontinues;
		ctx.m.mbreaks <- oldb;
		ctx.m.mcontinues <- oldc;
		alloc_tmp ctx HVoid
	| TWhile (cond,eloop,DoWhile) ->
		let oldb = ctx.m.mbreaks and oldc = ctx.m.mcontinues in
		ctx.m.mbreaks <- [];
		ctx.m.mcontinues <- [];
		let start = jump ctx (fun p -> OJAlways p) in
		let continue_pos = current_pos ctx in
		let ret = jump_back ctx in
		let j = jump_expr ctx cond false in
		start();
		ignore(eval_expr ctx eloop);
		ret();
		j();
		List.iter (fun f -> f (current_pos ctx)) ctx.m.mbreaks;
		List.iter (fun f -> f continue_pos) ctx.m.mcontinues;
		ctx.m.mbreaks <- oldb;
		ctx.m.mcontinues <- oldc;
		alloc_tmp ctx HVoid
	| TCast (v,None) ->
		eval_to ctx v (to_type ctx e.etype)
	| TArrayDecl el ->
		let r = alloc_tmp ctx (to_type ctx e.etype) in
		let et = (match follow e.etype with TInst (_,[t]) -> to_type ctx t | _ -> assert false) in
		(match et with
		| HI32 ->
			let b = alloc_tmp ctx HBytes in
			let size = reg_int ctx ((List.length el) * 4) in
			op ctx (OCall1 (b,alloc_std ctx "balloc" [HI32] HBytes,size));
			list_iteri (fun i e ->
				let r = eval_to ctx e HI32 in
				op ctx (OSetI32 (b,reg_int ctx (i * 4),r));
			) el;
			op ctx (OCall2 (r, alloc_fun_path ctx (["hl";"types"],"ArrayI32") "alloc", b, reg_int ctx (List.length el)));
		| HF64 ->
			let b = alloc_tmp ctx HBytes in
			let size = reg_int ctx ((List.length el) * 8) in
			op ctx (OCall1 (b,alloc_std ctx "balloc" [HI32] HBytes,size));
			list_iteri (fun i e ->
				let r = eval_to ctx e HF64 in
				op ctx (OSetF64 (b,reg_int ctx (i * 8),r));
			) el;
			op ctx (OCall2 (r, alloc_fun_path ctx (["hl";"types"],"ArrayF64") "alloc", b, reg_int ctx (List.length el)));
		| _ ->
			let at = if safe_cast et (HDyn None) then et else HDyn None in
			let a = alloc_tmp ctx HArray in
			let rt = alloc_tmp ctx HType in
			op ctx (OType (rt,et));
			let size = reg_int ctx (List.length el) in
			op ctx (OCall2 (a,alloc_std ctx "aalloc" [HType;HI32] HArray,rt,size));
			list_iteri (fun i e ->
				let r = eval_to ctx e at in
				op ctx (OSetArray (a,reg_int ctx i,r));
			) el;
			op ctx (OCall1 (r, alloc_fun_path ctx (["hl";"types"],"ArrayObj") "alloc", a)));
		r
	| TArray (a,i) ->
		let ra = eval_null_check ctx a in
		let ri = eval_to ctx i HI32 in
		let ra, at = (match follow a.etype with
			| TInst ({ cl_path = [],"Array" },[t]) -> ra, to_type ctx t
			| t when t == t_dynamic ->
				let at = e.etype in
				let aa = alloc_tmp ctx (to_type ctx (ctx.com.basic.tarray at)) in
				op ctx (OSafeCast (aa,ra));
				aa, to_type ctx at
			| _ ->
				error ("Invalid array access on " ^ s_type (print_context()) a.etype) a.epos
		) in
		(match at with
		| HI32 ->
			let hbytes = alloc_tmp ctx HBytes in
			op ctx (OField (hbytes, ra, 0));

			(* check bounds *)
			let size = alloc_tmp ctx HI32 in
			op ctx (OField (size, ra, 2));
			let r = alloc_tmp ctx at in
			let j = jump ctx (fun i -> OJULt (ri,size,i)) in
			op ctx (OInt (r,alloc_i32 ctx 0l));
			let jend = jump ctx (fun i -> OJAlways i) in
			j();
			let r2 = alloc_tmp ctx HI32 in
			op ctx (OInt (r2,alloc_i32 ctx 2l));
			op ctx (OShl (ri,ri,r2));
			op ctx (OGetI32 (r,hbytes,ri));
			jend();
			r
		| HF64 ->
			let hbytes = alloc_tmp ctx HBytes in
			op ctx (OField (hbytes, ra, 0));

			(* check bounds *)
			let size = alloc_tmp ctx HI32 in
			op ctx (OField (size, ra, 2));
			let r = alloc_tmp ctx at in
			let j = jump ctx (fun i -> OJULt (ri,size,i)) in
			op ctx (OFloat (r,alloc_float ctx 0.));
			let jend = jump ctx (fun i -> OJAlways i) in
			j();
			let r2 = alloc_tmp ctx HI32 in
			op ctx (OInt (r2,alloc_i32 ctx 3l));
			op ctx (OShl (ri,ri,r2));
			op ctx (OGetF64 (r,hbytes,ri));
			jend();
			r
		| _ ->
			let harr = alloc_tmp ctx HArray in
			op ctx (OField (harr, ra, 0));

			(* check bounds *)
			let size = alloc_tmp ctx HI32 in
			op ctx (OArraySize (size,harr));
			let r = alloc_tmp ctx at in
			let j = jump ctx (fun i -> OJULt (ri,size,i)) in
			op ctx (ONull r);
			let jend = jump ctx (fun i -> OJAlways i) in
			j();
			let tmp = alloc_tmp ctx (HDyn None) in
			op ctx (OGetArray (tmp,harr,ri));
			let r2 = unsafe_cast_to ctx tmp at e.epos in
			op ctx (OMov (r,r2));
			jend();
			r
		);
	| TMeta (_,e) ->
		eval_expr ctx e
	| TFor _ ->
		assert false (* eliminated with pf_for_to_while *)
	| TSwitch (en,cases,def) ->
		let rt = to_type ctx e.etype in
		let r = alloc_tmp ctx rt in
		(try
			let max = ref (-1) in
			let rec get_int e =
				match e.eexpr with
				| TConst (TInt i) ->
					let v = Int32.to_int i in
					if Int32.of_int v <> i then raise Exit;
					v
				| _ ->
					raise Exit
			in
			List.iter (fun (values,_) ->
				List.iter (fun v ->
					let i = get_int v in
					if i < 0 then raise Exit;
					if i > !max then max := i;
				) values;
			) cases;
			if !max > 255 then raise Exit;
			let ridx = eval_to ctx en HI32 in
			let indexes = Array.make (!max + 1) 0 in
			op ctx (OSwitch (ridx,indexes));
			let switch_pos = current_pos ctx in
			(match def with
			| None ->
				op ctx (ONull r);
			| Some e ->
				let re = eval_to ctx e rt in
				op ctx (OMov (r,re)));
			let jends = ref [jump ctx (fun i -> OJAlways i)] in
			List.iter (fun (values,ecase) ->
				List.iter (fun v ->
					Array.set indexes (get_int v) (current_pos ctx - switch_pos)
				) values;
				let re = eval_to ctx ecase rt in
				op ctx (OMov (r,re));
				jends := jump ctx (fun i -> OJAlways i) :: !jends
			) cases;
			List.iter (fun j -> j()) (!jends);
		with Exit ->
			let jends = ref [] in
			let rvalue = eval_expr ctx en in
			let rec loop next (cases,e) =
				let next = List.fold_left (fun next c ->
					next();
					let r = eval_expr ctx c in
					let rv = cast_to ctx rvalue (rtype ctx r) e.epos in
					let j = jump ctx (fun n -> OJNeq (r,rv,n)) in
					j
				) next cases in
				let re = eval_to ctx e rt in
				if rt <> HVoid then op ctx (OMov (r,re));
				next
			in
			let j = List.fold_left loop (fun() -> ()) cases in
			j();
			(match def with
			| None -> if rt <> HVoid then op ctx (ONull r)
			| Some e ->
				let rdef = eval_to ctx e rt in
				if rt <> HVoid then op ctx (OMov (r,rdef)));
			List.iter (fun j -> j()) (!jends);
		);
		r
	| TEnumParameter (ec,f,index) ->
		let r = alloc_tmp ctx (to_type ctx e.etype) in
		op ctx (OEnumField (r,eval_expr ctx ec,f.ef_index,index));
		r
	| TContinue ->
		let pos = current_pos ctx in
		op ctx (OJAlways (-1)); (* loop *)
		ctx.m.mcontinues <- (fun target -> DynArray.set ctx.m.mops pos (OJAlways (target - (pos + 1)))) :: ctx.m.mcontinues;
		alloc_tmp ctx HVoid
	| TBreak ->
		let pos = current_pos ctx in
		op ctx (OJAlways (-1)); (* loop *)
		ctx.m.mbreaks <- (fun target -> DynArray.set ctx.m.mops pos (OJAlways (target - (pos + 1)))) :: ctx.m.mbreaks;
		alloc_tmp ctx HVoid
	| TTry (etry,catches) ->
		let pos = current_pos ctx in
		let rtrap = alloc_tmp ctx (HDyn None) in
		op ctx (OTrap (rtrap,-1)); (* loop *)
		ctx.m.mtrys <- ctx.m.mtrys + 1;
		let tret = to_type ctx e.etype in
		let result = alloc_tmp ctx tret in
		let r = eval_expr ctx etry in
		if tret <> HVoid then op ctx (OMov (result,cast_to ctx r tret etry.epos));
		ctx.m.mtrys <- ctx.m.mtrys - 1;
		op ctx (OEndTrap 0);
		let j = jump ctx (fun n -> OJAlways n) in
		DynArray.set ctx.m.mops pos (OTrap (rtrap, current_pos ctx - (pos + 1)));
		let rec loop l =
			match l with
			| [] -> assert false
			| (v,ec) :: next ->
				let rv = alloc_reg ctx v in
				if v.v_type == t_dynamic then
					op ctx (OMov (rv, rtrap))
				else
					error "Unsupported catch" ec.epos;
				let r = eval_expr ctx ec in
				if tret <> HVoid then op ctx (OMov (result,cast_to ctx r tret ec.epos));
				if next = [] then [] else jump ctx (fun n -> OJAlways n) :: loop next
		in
		List.iter (fun j -> j()) (loop catches);
		j();
		result
	| TTypeExpr t ->
		let r = alloc_tmp ctx HType in
		op ctx (OType (r, (match t with
			| TClassDecl c -> class_type ctx c []
			| TEnumDecl e -> enum_type ctx e
			| _ -> assert false)));
		r
	| TCast (ev,Some t) ->
		let r = alloc_tmp ctx (to_type ctx (match t with TClassDecl c -> TInst (c,List.map (fun _ -> t_dynamic) c.cl_params) | _ -> assert false)) in
		let re = eval_expr ctx ev in
		if safe_cast (rtype ctx re) (rtype ctx r) then
			op ctx (OMov (r,re))
		else
			error "TODO : safe-cast" e.epos;
		r

and build_capture_vars ctx f =
	let ignored_vars = ref PMap.empty in
	let used_vars = ref PMap.empty in
	(* get all captured vars in scope, ignore vars that are declared *)
	let rec loop e =
		let decl_var v =
			if v.v_capture then ignored_vars := PMap.add v.v_id () !ignored_vars
		in
		let use_var v =
			if v.v_capture then used_vars := PMap.add v.v_id v !used_vars
		in
		(match e.eexpr with
		| TLocal v ->
			use_var v;
		| TVar (v,_) ->
			decl_var v
		| TTry (_,catches) ->
			List.iter (fun (v,_) -> decl_var v) catches
		| TFunction f ->
			List.iter (fun (v,_) -> decl_var v) f.tf_args;
		| _ ->
			()
		);
		Type.iter loop e
	in
	loop f.tf_expr;
	let cvars = Array.of_list (PMap.fold (fun v acc -> if PMap.mem v.v_id !ignored_vars then acc else v :: acc) !used_vars []) in
	Array.sort (fun v1 v2 -> v1.v_id - v2.v_id) cvars;
	let indexes = ref PMap.empty in
	Array.iteri (fun i v -> indexes := PMap.add v.v_id i !indexes) cvars;
	{
		c_map = !indexes;
		c_vars = cvars;
		c_type = HEnum {
			ename = "";
			eid = 0;
			efields = [|"",0,Array.map (fun v -> to_type ctx v.v_type) cvars|];
		};
	}

and gen_method_wrapper ctx rt t p =
	try
		PMap.find (rt,t) ctx.method_wrappers
	with Not_found ->
		let fid = lookup_alloc ctx.cfids () in
		ctx.method_wrappers <- PMap.add (rt,t) fid ctx.method_wrappers;
		let old = ctx.m in
		let targs, tret = (match t with HFun (args, ret) -> args, ret | _ -> assert false) in
		let iargs, iret = (match rt with HFun (args, ret) -> args, ret | _ -> assert false) in
		ctx.m <- method_context (HDyn None) null_capture;
		let rfun = alloc_tmp ctx rt in
		let rargs = List.map (alloc_tmp ctx) targs in
		let rret = alloc_tmp ctx iret in
		op ctx (OCallClosure (rret,rfun,List.map2 (fun r t -> cast_to ctx r t p) rargs iargs));
		op ctx (ORet (cast_to ctx rret tret p));
		let f = {
			findex = fid;
			ftype = HFun (rt :: targs, tret);
			regs = DynArray.to_array ctx.m.mregs.arr;
			code = DynArray.to_array ctx.m.mops;
			debug = DynArray.to_array ctx.m.mdebug;
		} in
		ctx.m <- old;
		DynArray.add ctx.cfunctions f;
		fid

and make_fun ctx fidx f cthis cparent =
	let old = ctx.m in
	let capt = build_capture_vars ctx f in
	let has_captured_vars = Array.length capt.c_vars > 0 in
	let capt, use_parent_capture = (match cparent with
		| Some cparent when has_captured_vars && List.for_all (fun v -> PMap.mem v.v_id cparent.c_map) (Array.to_list capt.c_vars) -> cparent, true
		| _ -> capt, false
	) in

	ctx.m <- method_context (to_type ctx f.tf_type) capt;

	let tthis = (match cthis with
	| None -> None
	| Some c ->
		let t = to_type ctx (TInst (c,[])) in
		ignore(alloc_tmp ctx t); (* index 0 *)
		Some t
	) in

	let rcapt = if has_captured_vars && cparent <> None then Some (alloc_tmp ctx capt.c_type) else None in

	let args = List.map (fun (v,o) ->
		let r = alloc_reg ctx v in
		rtype ctx r
	) f.tf_args in

	if has_captured_vars then ctx.m.mcaptreg <- (match rcapt with
		| None ->
			let r = alloc_tmp ctx capt.c_type in
			op ctx (OEnumAlloc (r,0));
			r
		| Some r -> r
	);

	List.iter (fun (v, o) ->
		let r = alloc_reg ctx v in
		(match o with
		| None | Some TNull -> ()
		| Some c ->
			op ctx (OJNotNull (r,1));
			match c with
			| TNull | TThis | TSuper -> assert false
			| TInt i -> op ctx (OInt (r, alloc_i32 ctx i))
			| TFloat s -> op ctx (OFloat (r, alloc_float ctx (float_of_string s)))
			| TBool b -> op ctx (OBool (r, b))
			| TString s -> assert false (* TODO *)
		);
		if v.v_capture then begin
			let index = (try PMap.find v.v_id capt.c_map with Not_found -> assert false) in
			op ctx (OSetEnumField (ctx.m.mcaptreg, index, r));
		end
	) f.tf_args;

	ignore(eval_expr ctx f.tf_expr);
	let tret = to_type ctx f.tf_type in
	let rec has_final_jump e =
		match e.eexpr with
		| TBlock el -> (match List.rev el with e :: _ -> has_final_jump e | [] -> false)
		| TParenthesis e -> has_final_jump e
		| TReturn _ -> false
		| _ -> true
	in
	if tret = HVoid then
		op ctx (ORet (alloc_tmp ctx HVoid))
	else if has_final_jump f.tf_expr then begin
		let r = alloc_tmp ctx tret in
		(match tret with
		| HI32 | HI8 | HI16 -> op ctx (OInt (r,alloc_i32 ctx 0l))
		| HF32 | HF64 -> op ctx (OFloat (r,alloc_float ctx 0.))
		| _ -> op ctx (ONull r));
		op ctx (ORet r)
	end;
	let fargs = (match tthis with None -> [] | Some t -> [t]) @ (match rcapt with None -> [] | Some r -> [rtype ctx r]) @ args in
	let f = {
		findex = fidx;
		ftype = HFun (fargs, tret);
		regs = DynArray.to_array ctx.m.mregs.arr;
		code = DynArray.to_array ctx.m.mops;
		debug = DynArray.to_array ctx.m.mdebug;
	} in
	ctx.m <- old;
	Hashtbl.add ctx.defined_funs fidx ();
	DynArray.add ctx.cfunctions f;
	capt

let generate_static ctx c f =
	match f.cf_kind with
	| Var _ | Method MethDynamic ->
		()
	| Method m ->
		let rec loop = function
			| (Meta.Custom ":hlNative",[(EConst(String(lib)),_);(EConst(String(name)),_)] ,_ ) :: _ ->
				ignore(lookup ctx.cnatives (name ^ "@" ^ lib) (fun() ->
					let fid = alloc_fid ctx c f in
					Hashtbl.add ctx.defined_funs fid ();
					(alloc_string ctx lib, alloc_string ctx name,to_type ctx f.cf_type,fid)
				));
			| (Meta.Custom ":hlNative",_ ,p) :: _ ->
				error "Invalid @:hlNative decl" p
			| [] ->
				ignore(make_fun ctx (alloc_fid ctx c f) (match f.cf_expr with Some { eexpr = TFunction f } -> f | _ -> assert false) None None)
			| _ :: l ->
				loop l
		in
		loop f.cf_meta


let generate_member ctx c f =
	match f.cf_kind with
	| Var _ | Method MethDynamic -> ()
	| Method m ->
		ignore(make_fun ctx (alloc_fid ctx c f) (match f.cf_expr with Some { eexpr = TFunction f } -> f | _ -> error "Missing function body" f.cf_pos) (Some c) None);
		if f.cf_name = "toString" && not (List.memq f c.cl_overrides) && not (PMap.mem "__string" c.cl_fields) then begin
			let p = f.cf_pos in
			(* function __string() return this.toString().bytes *)
			let ethis = mk (TConst TThis) (TInst (c,List.map snd c.cl_params)) p in
			let tstr = mk (TCall (mk (TField (ethis,FInstance(c,List.map snd c.cl_params,f))) f.cf_type p,[])) ctx.com.basic.tstring p in
			let cstr, cf_bytes = (try (match ctx.com.basic.tstring with TInst(c,_) -> c, PMap.find "bytes" c.cl_fields | _ -> assert false) with Not_found -> assert false) in
			let estr = mk (TReturn (Some (mk (TField (tstr,FInstance (cstr,[],cf_bytes))) cf_bytes.cf_type p))) ctx.com.basic.tvoid p in
			ignore(make_fun ctx (alloc_fun_path ctx c.cl_path "__string") { tf_expr = estr; tf_args = []; tf_type = cf_bytes.cf_type; } (Some c) None)
		end

let generate_enum ctx e =
	()

let generate_type ctx t =
	match t with
	| TClassDecl { cl_interface = true }->
		()
	| TClassDecl c when c.cl_extern ->
		List.iter (fun f ->
			List.iter (fun (name,args,pos) ->
				match name with
				| Meta.Custom ":hlNative" -> generate_static ctx c f
				| _ -> ()
			) f.cf_meta
		) c.cl_ordered_statics
	| TClassDecl c ->
		List.iter (generate_static ctx c) c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f -> generate_member ctx c f);
		List.iter (generate_member ctx c) c.cl_ordered_fields;
	| TTypeDecl _ | TAbstractDecl _ ->
		()
	| TEnumDecl e  ->
		generate_enum ctx e

let generate_static_init ctx =
	let exprs = ref [] in
	let t_void = ctx.com.basic.tvoid in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			List.iter (fun f ->
				match f.cf_kind, f.cf_expr with
				| Var _, Some e | Method MethDynamic, Some e ->
					let p = e.epos in
					let e = mk (TBinop (OpAssign,(mk (TField (mk (TTypeExpr t) t_dynamic p,FStatic (c,f))) f.cf_type p), e)) f.cf_type p in
					exprs := e :: !exprs;
				| _ ->
					()
			) c.cl_ordered_statics;
		| _ -> ()
	) ctx.com.types;
	(match ctx.com.main_class with
	| None -> ()
	| Some m ->
		let t = (try List.find (fun t -> t_path t = m) ctx.com.types with Not_found -> assert false) in
		match t with
		| TClassDecl c ->
			let f = (try PMap.find "main" c.cl_statics with Not_found -> assert false) in
			let p = { pfile = "<startup>"; pmin = 0; pmax = 0; } in
			exprs := mk (TCall (mk (TField (mk (TTypeExpr t) t_dynamic p, FStatic (c,f))) f.cf_type p,[])) t_void p :: !exprs
		| _ ->
			assert false
	);
	let fid = alloc_function_name ctx "<entry>" in
	ignore(make_fun ctx fid { tf_expr = mk (TBlock (List.rev !exprs)) t_void null_pos; tf_args = []; tf_type = t_void } None None);
	fid


(* ------------------------------- CHECK ---------------------------------------------- *)

let check code =
	let ftypes = Array.create (Array.length code.natives + Array.length code.functions) HVoid in
	let is_native_fun = Hashtbl.create 0 in

	let check_fun f =
		let pos = ref 0 in
		let error msg =
			failwith ("In function " ^ string_of_int f.findex ^ "@" ^ string_of_int (!pos) ^ " : " ^ msg)
		in
		let targs, tret = (match f.ftype with HFun (args,ret) -> args, ret | _ -> assert false) in
		let rtype i = f.regs.(i) in
		let check t1 t2 =
			if not (safe_cast t1 t2) then error (tstr t1 ^ " should be " ^ tstr t2)
		in
		let reg_inf r =
			"Register " ^ string_of_int r ^ "(" ^ tstr (rtype r) ^ ")"
		in
		let reg r t =
			if not (safe_cast (rtype r) t) then error (reg_inf r ^ " should be " ^ tstr t ^ " and not " ^ tstr (rtype r))
		in
		let numeric r =
			match rtype r with
			| HI8 | HI16 | HI32 | HF32 | HF64 -> ()
			| _ -> error (reg_inf r ^ " should be numeric")
		in
		let int r =
			match rtype r with
			| HI8 | HI16 | HI32 -> ()
			| _ -> error (reg_inf r ^ " should be integral")
		in
		let float r =
			match rtype r with
			| HF32 | HF64 -> ()
			| _ -> error (reg_inf r ^ " should be float")
		in
		let call f args r =
			match ftypes.(f) with
			| HFun (targs, tret) ->
				if List.length args <> List.length targs then error (tstr (HFun (List.map rtype args, rtype r)) ^ " should be " ^ tstr ftypes.(f));
				List.iter2 reg args targs;
				reg r tret
			| _ -> assert false
		in
		let can_jump delta =
			if !pos + 1 + delta < 0 || !pos + 1 + delta >= Array.length f.code then error "Jump outside function bounds";
			if delta < 0 && Array.get f.code (!pos + 1 + delta) <> OLabel 0 then error "Jump back without Label";
		in
		let is_obj r =
			match rtype r with
			| HObj _ -> ()
			| _ -> error (reg_inf r ^ " should be object")
		in
		let is_enum r =
			match rtype r with
			| HEnum _ -> ()
			| _ -> error (reg_inf r ^ " should be enum")
		in
		let is_dyn r =
			if not (safe_cast (rtype r) (HDyn None)) then error (reg_inf r ^ " should be castable to dynamic")
		in
		let tfield o id proto =
			match rtype o with
			| HObj p ->
				let rec loop pl p =
					let pl = p :: pl in
					match p.psuper with
					| None ->
						let rec fetch id = function
							| [] -> assert false
							| p :: pl ->
								let d = id - Array.length p.pfields in
								if d < 0 then
									let _, _, t = p.pfields.(id) in
									t
								else
									fetch d pl
						in
						fetch id pl
					| Some p ->
						loop pl p
				in
				if proto then ftypes.(p.pvirtuals.(id)) else loop [] p
			| HVirtual v when not proto ->
				let _,_, t = v.vfields.(id) in
				t
			| _ ->
				is_obj o;
				HVoid
		in
		iteri reg targs;
		Array.iteri (fun i op ->
			pos := i;
			match op with
			| OMov (a,b) ->
				reg b (rtype a)
			| OInt (r,i) ->
				ignore(code.ints.(i));
				int r
			| OFloat (r,i) ->
				if rtype r <> HF32 then reg r HF64;
				if i < 0 || i >= Array.length code.floats then error "float outside range";
			| OBool (r,_) ->
				reg r HBool
			| OString (r,i) ->
				reg r HBytes;
				if i < 0 || i >= Array.length code.strings then error "string outside range";
			| ONull r ->
				(match rtype r with
				| HBytes | HEnum _ | HVirtual _ | HType -> ()
				| _ when safe_cast (rtype r) (HDyn None) -> ()
				| t -> error (tstr t ^ " is not nullable"))
			| OAdd (r,a,b) | OSub (r,a,b) | OMul (r,a,b) | OSDiv (r,a,b) | OUDiv (r,a,b) | OSMod (r,a,b) | OUMod(r,a,b) ->
				numeric r;
				reg a (rtype r);
				reg b (rtype r);
			| ONeg (r,a) ->
				numeric r;
				reg a (rtype r);
			| OShl (r,a,b) | OSShr (r,a,b) | OUShr (r,a,b) | OAnd (r,a,b) | OOr (r,a,b) | OXor (r,a,b) ->
				int r;
				reg a (rtype r);
				reg b (rtype r);
			| OIncr r ->
				int r
			| ODecr r ->
				int r
			| ONot (a,b) ->
				reg a HBool;
				reg b HBool;
			| OCall0 (r,f) ->
				call f [] r
			| OCall1 (r, f, a) ->
				call f [a] r
			| OCall2 (r, f, a, b) ->
				call f [a;b] r
			| OCall3 (r, f, a, b, c) ->
				call f [a;b;c] r
			| OCall4 (r, f, a, b, c, d) ->
				call f [a;b;c;d] r
			| OCallN (r,f,rl) ->
				call f rl r
			| OCallThis (r, m, rl) ->
				(match tfield 0 m true with
				| HFun (tobj :: targs, tret) when List.length targs = List.length rl -> reg 0 tobj; List.iter2 reg rl targs; reg r tret
				| t -> check t (HFun (rtype 0 :: List.map rtype rl, rtype r)));
			| OCallMethod (r, m, rl) ->
				(match rl with
				| [] -> assert false
				| obj :: _ ->
					match tfield obj m true with
					| HFun (targs, tret) when List.length targs = List.length rl -> List.iter2 reg rl targs; reg r tret
					| t -> check t (HFun (List.map rtype rl, rtype r)));
			| OCallClosure (r,f,rl) ->
				(match rtype f with
				| HFun (targs,tret) when List.length targs = List.length rl -> List.iter2 reg rl targs; reg r tret
				| _ -> reg f (HFun(List.map rtype rl,rtype r)))
			| OGetGlobal (r,g) | OSetGlobal (g,r) ->
				reg r code.globals.(g)
			| OSLt (r, a, b) | OULt (r, a, b) | OSGte (r, a, b) | OUGte (r, a, b) ->
				reg r HBool;
				reg a (rtype b)
			| OEq (r,a,b) | ONotEq (r, a, b) ->
				reg r HBool;
				if not (safe_cast (rtype b) (rtype a)) then reg a (rtype b)
			| ORet r ->
				reg r tret
			| OJTrue (r,delta) | OJFalse (r,delta) ->
				reg r HBool;
				can_jump delta
			| OJNull (r,delta) | OJNotNull (r,delta) ->
				ignore(rtype r);
				can_jump delta
			| OJUGte (a,b,delta) | OJULt (a,b,delta) | OJSGte (a,b,delta) | OJSLt (a,b,delta) ->
				reg a (rtype b);
				can_jump delta
			| OJEq (a,b,delta) | OJNeq (a,b,delta) ->
				if not (safe_cast (rtype b) (rtype a)) then reg a (rtype b);
				can_jump delta
			| OJAlways d ->
				can_jump d
			| OToDyn (r,a) ->
				if safe_cast (rtype a) (HDyn None) then reg a HI32; (* don't wrap as dynamic types that can safely be cast to it *)
				reg r (HDyn (Some (rtype a)))
			| OToFloat (a,b) ->
				int b;
				float a;
			| OToInt (a,b) ->
				int a;
				float b;
			| OLabel _ ->
				()
			| ONew r ->
				(match rtype r with
				| HDynObj -> ()
				| _ -> is_obj r)
			| OField (r,o,fid) | OSetField (o,fid,r) ->
				reg r (tfield o fid false)
			| OGetThis (r,fid) | OSetThis(fid,r) ->
				reg r (tfield 0 fid false)
			| OGetFunction (r,f) ->
				reg r ftypes.(f)
			| OMethod (r,o,fid) ->
				(match rtype o with
				| HObj _ ->
					(match tfield o fid true with
					| HFun (t :: tl, tret) ->
						reg o t;
						reg r (HFun (tl,tret));
					| _ ->
						assert false)
				| HVirtual v ->
					let _,_, t = v.vfields.(fid) in
					reg r t;
				| _ ->
					is_obj o)
			| OClosure (r,f,arg) ->
				(match ftypes.(f) with
				| HFun (t :: tl, tret) ->
					reg arg t;
					reg r (HFun (tl,tret));
				| _ -> assert false);
			| OThrow r ->
				reg r (HDyn None)
			| OGetArray (v,a,i) ->
				reg a HArray;
				reg i HI32;
				is_dyn v;
			| OGetI8 (r,b,p) ->
				reg r HI32;
				reg b HBytes;
				reg p HI32;
			| OGetI32 (r,b,p) ->
				reg r HI32;
				reg b HBytes;
				reg p HI32;
			| OGetF32 (r,b,p) ->
				reg r HF32;
				reg b HBytes;
				reg p HI32;
			| OGetF64 (r,b,p) ->
				reg r HF64;
				reg b HBytes;
				reg p HI32;
			| OSetI8 (r,p,v) ->
				reg r HBytes;
				reg p HI32;
				reg v HI32;
			| OSetI32 (r,p,v) ->
				reg r HBytes;
				reg p HI32;
				reg v HI32;
			| OSetF32 (r,p,v) ->
				reg r HBytes;
				reg p HI32;
				reg v HF32;
			| OSetF64 (r,p,v) ->
				reg r HBytes;
				reg p HI32;
				reg v HF64;
			| OSetArray (a,i,v) ->
				reg a HArray;
				reg i HI32;
				is_dyn v;
			| OUnsafeCast (a,b) ->
				if not (safe_cast (rtype a) (HDyn None)) then is_obj a;
				ignore(rtype b);
			| OSafeCast (a,b) ->
				ignore(rtype a);
				ignore(rtype b);
			| OArraySize (r,a) ->
				reg a HArray;
				reg r HI32
			| OError s ->
				ignore(code.strings.(s));
			| OType (r,_) ->
				reg r HType
			| ORef (r,v) ->
				reg r (HRef (rtype v))
			| OUnref (v,r) ->
				(match rtype r with
				| HRef t -> reg v t
				| _ -> reg r (HRef (rtype v)))
			| OSetref (r,v) ->
				reg r (HRef (rtype v))
			| OToVirtual (r,v) ->
				(match rtype r with
				| HVirtual _ -> ()
				| _ -> reg r (HVirtual {vfields=[||];vindex=PMap.empty;}));
				(match rtype v with
				| HObj _ | HDynObj | HDyn None -> ()
				| _ -> reg v HDynObj)
			| OUnVirtual (r,v) ->
				(match rtype v with
				| HVirtual _ -> ()
				| _ -> reg r (HVirtual {vfields=[||];vindex=PMap.empty;}));
				reg r (HDyn None)
			| ODynGet (v,r,f) | ODynSet (r,f,v) ->
				ignore(code.strings.(f));
				ignore(rtype v);
				(match rtype r with
				| HObj _ | HDyn None | HDynObj | HVirtual _ -> ()
				| _ -> reg r HDynObj)
			| OMakeEnum (r,index,pl) ->
				(match rtype r with
				| HEnum e ->
					let _,_, fl = e.efields.(index) in
					if Array.length fl <> List.length pl then error ("MakeEnum has " ^ (string_of_int (List.length pl)) ^ " params while " ^ (string_of_int (Array.length fl)) ^ " are required");
					List.iter2 (fun r t -> reg r t) pl (Array.to_list fl)
				| _ ->
					is_enum r)
			| OEnumAlloc (r,index) ->
				(match rtype r with
				| HEnum e ->
					ignore(e.efields.(index))
				| _ ->
					is_enum r)
			| OEnumIndex (r,v) ->
				is_enum v;
				reg r HI32;
			| OEnumField (r,e,f,i) ->
				(match rtype e with
				| HEnum e ->
					let _, _, tl = e.efields.(f) in
					check (rtype r) tl.(i)
				| _ -> is_enum e)
			| OSetEnumField (e,i,r) ->
				(match rtype e with
				| HEnum e ->
					let _, _, tl = e.efields.(0) in
					check (rtype r) tl.(i)
				| _ -> is_enum e)
			| OSwitch (r,idx) ->
				reg r HI32;
				Array.iter can_jump idx
			| ONullCheck r ->
				ignore(rtype r)
			| OTrap (r, idx) ->
				reg r (HDyn None);
				can_jump idx
			| OEndTrap _ ->
				()
			| ODump r ->
				ignore(rtype r);
		) f.code
		(* TODO : check that all path correctly initialize NULL values and reach a return *)
	in
	Array.iter (fun fd ->
		if fd.findex >= Array.length ftypes then failwith ("Invalid function index " ^ string_of_int fd.findex);
		if ftypes.(fd.findex) <> HVoid then failwith ("Duplicate function bind " ^ string_of_int fd.findex);
		ftypes.(fd.findex) <- fd.ftype;
	) code.functions;
	Array.iter (fun (_,_,t,idx) ->
		if idx >= Array.length ftypes then failwith ("Invalid native function index " ^ string_of_int idx);
		if ftypes.(idx) <> HVoid then failwith ("Duplicate native function bind " ^ string_of_int idx);
		Hashtbl.add is_native_fun idx true;
		ftypes.(idx) <- t
	) code.natives;
	(* TODO : check that no object type has a virtual native in his proto *)
	Array.iter check_fun code.functions

(* ------------------------------- INTERP --------------------------------------------- *)

type value =
	| VNull
	| VInt of int32
	| VFloat of float
	| VBool of bool
	| VDyn of value * ttype
	| VObj of vobject
	| VClosure of vfunction * value option
	| VBytes of string
	| VArray of value array * ttype
	| VUndef
	| VType of ttype
	| VRef of value array * int * ttype
	| VVirtual of vvirtual
	| VDynObj of vdynobj
	| VEnum of int * value array
	| VAbstract of vabstract

and vabstract =
	| AHashBytes of (string, value) Hashtbl.t

and vfunction =
	| FFun of fundecl
	| FNativeFun of string * (value list -> value)

and vobject = {
	oproto : vproto;
	ofields : value array;
}

and vproto = {
	pclass : class_proto;
	pmethods : vfunction array;
}

and vvirtual = {
	vtype : virtual_proto;
	vindexes : vfield array;
	vtable : value array;
	vvalue : value;
}

and vdynobj = {
	dfields : (string, int) Hashtbl.t;
	mutable dtypes : ttype array;
	mutable dvalues : value array;
	mutable dvirtuals : vvirtual list;
}

and vfield =
	| VFNone
	| VFIndex of int

exception Return of value

let default t =
	match t with
	| HVoid | HFun _ | HDyn _ | HObj _ | HBytes | HArray | HType | HRef _ | HVirtual _ | HDynObj | HAbstract _ | HEnum _ -> VNull
	| HI8 | HI16 | HI32 -> VInt Int32.zero
	| HF32 | HF64 -> VFloat 0.
	| HBool -> VBool false

let is_compatible v t =
	match v, t with
	| VInt _, HI32 -> true
	| VBool _, HBool -> true
	| VNull, (HObj _ | HFun _ | HBytes | HArray | HType | HVirtual _ | HDynObj | HAbstract _ | HEnum _ | HDyn _) -> true
	| VObj _, HObj _ -> true
	| VClosure _, HFun _ -> true
	| VBytes _, HBytes -> true
	| VDyn (_,t1), HDyn (Some t2) -> tsame t1 t2
	| (VDyn _ | VObj _), HDyn None -> true
	| VUndef, HVoid -> true
	| VType _, HType -> true
	| VArray _, HArray -> true
	| VDynObj _, HDynObj -> true
	| VVirtual v, HVirtual vt -> v.vtype == vt
	| VRef (_,_,t1), HRef t2 -> tsame t1 t2
	| _ -> false

exception Runtime_error of string
exception InterpThrow of value

let interp code =

	let globals = Array.map default code.globals in
	let functions = Array.create (Array.length code.functions + Array.length code.natives) (FNativeFun ("",(fun _ -> assert false))) in
	let cached_protos = Hashtbl.create 0 in
	let func f = Array.unsafe_get functions f in

	let stack = ref [] in
	let exc_stack = ref [] in

	let rec get_proto p =
		try
			Hashtbl.find cached_protos p.pname
		with Not_found ->
			let meths, fields = (match p.psuper with None -> [||],[||] | Some p -> let p,f = get_proto p in p.pmethods, f) in
			let meths = Array.append meths (Array.map (fun f -> functions.(f)) p.pvirtuals) in
			let fields = Array.append fields (Array.map (fun (_,_,t) -> t) p.pfields) in
			let proto = ({ pclass = p; pmethods = meths },fields) in
			Hashtbl.replace cached_protos p.pname proto;
			proto
	in

	let error msg = raise (Runtime_error msg) in
	let throw v = exc_stack := []; raise (InterpThrow v) in

	let rec vstr_d v =
		match v with
		| VNull -> "null"
		| VInt i -> Int32.to_string i ^ "i"
		| VFloat f -> string_of_float f ^ "f"
		| VBool b -> if b then "true" else "false"
		| VDyn (v,t) -> "dyn(" ^ vstr_d v ^ ")"
		| VObj o ->
			let p = "#" ^ o.oproto.pclass.pname in
			let fid = ref None in
			Array.iter (fun p -> if p.fname = "__string" then fid := Some p.fmethod) o.oproto.pclass.pproto;
			(match !fid with
			| None -> p
			| Some f -> p ^ ":" ^ vstr_d (fcall (func f) [v]))
		| VBytes b -> "bytes(" ^ (if String.length b > 0 && String.get b (String.length b - 1) = '\x00' then String.sub b 0 (String.length b - 1) else b) ^ ")"
		| VClosure (f,o) ->
			(match o with
			| None -> fstr f
			| Some v -> fstr f ^ "(" ^ vstr_d v ^ ")")
		| VArray (a,t) -> "array<" ^ tstr t ^ ">(" ^ String.concat "," (Array.to_list (Array.map vstr_d a)) ^ ")"
		| VUndef -> "undef"
		| VType t -> "type(" ^ tstr t ^ ")"
		| VRef (regs,i,_) -> "ref(" ^ vstr_d regs.(i) ^ ")"
		| VVirtual v -> "virtual(" ^ vstr_d v.vvalue ^ ")"
		| VDynObj d -> "dynobj(" ^ String.concat "," (Hashtbl.fold (fun f i acc -> (f^":"^vstr_d d.dvalues.(i)) :: acc) d.dfields []) ^ ")"
		| VEnum (i,vals) -> "enum#" ^ string_of_int i  ^ "(" ^ String.concat "," (Array.to_list (Array.map vstr_d vals)) ^ ")"
		| VAbstract _ -> "abstract"

	and vstr v t =
		match v with
		| VNull -> "null"
		| VInt i -> Int32.to_string i
		| VFloat f -> string_of_float f
		| VBool b -> if b then "true" else "false"
		| VDyn (v,t) ->
			vstr v t
		| VObj o ->
			let fid = ref None in
			Array.iter (fun p -> if p.fname = "__string" then fid := Some p.fmethod) o.oproto.pclass.pproto;
			(match !fid with
			| None -> "#" ^ o.oproto.pclass.pname
			| Some f -> vstr (fcall (func f) [v]) HBytes)
		| VBytes b -> (if String.length b > 0 && String.get b (String.length b - 1) = '\x00' then String.sub b 0 (String.length b - 1) else b)
		| VClosure (f,_) -> fstr f
		| VArray (a,t) -> "[" ^ String.concat ", " (Array.to_list (Array.map (fun v -> vstr v t) a)) ^ "]"
		| VUndef -> "undef"
		| VType t -> tstr t
		| VRef (regs,i,t) -> "*" ^ (vstr regs.(i) t)
		| VVirtual v -> vstr v.vvalue (HDyn None)
		| VDynObj d -> "{" ^ String.concat ", " (Hashtbl.fold (fun f i acc -> (f^":"^vstr d.dvalues.(i) d.dtypes.(i)) :: acc) d.dfields []) ^ "}"
		| VAbstract _ -> "abstract"
		| VEnum (i,vals) ->
			(match t with
			| HEnum e ->
				let n, _, pl = e.efields.(i) in
				if Array.length pl = 0 then
					n
				else
					n ^ "(" ^ String.concat "," (List.map2 vstr (Array.to_list vals) (Array.to_list pl)) ^ ")"
			| _ ->
				assert false)

	and fstr = function
		| FFun f -> "function@" ^ string_of_int f.findex
		| FNativeFun (s,_) -> "native[" ^ s ^ "]"

	and fcall f args =
		match f with
		| FFun f -> call f args
		| FNativeFun (_,f) -> f args

	and call f args =
		let regs = Array.create (Array.length f.regs) VUndef in
		let pos = ref 0 in
		stack := (f,pos) :: !stack;
		let rtype i = f.regs.(i) in
		let check v t id =
			if not (is_compatible v t) then error (Printf.sprintf "Can't set %s(%s) with %s" (id()) (tstr t) (vstr_d v));
		in
		let check_obj v o fid =
			match o with
			| VObj o ->
				let _, fields = get_proto o.oproto.pclass in
				check v fields.(fid) (fun() -> "obj field")
			| VVirtual vp ->
				let _,_, t = vp.vtype.vfields.(fid) in
				check v t (fun() -> "virtual field")
			| _ ->
				()
		in
		let set r v =
			check v (rtype r) (fun() -> "register " ^ string_of_int r);
			Array.unsafe_set regs r v
		in
		iteri set args;
		let get r = Array.unsafe_get regs r in
		let global g = Array.unsafe_get globals g in
		let traps = ref [] in
		let numop iop fop a b =
			match rtype a with
			(* todo : sign-extend and mask after result for HI8/16 *)
			| HI8 | HI16 | HI32 ->
				(match regs.(a), regs.(b) with
				| VInt a, VInt b -> VInt (iop a b)
				| _ -> assert false)
			| HF32 | HF64 ->
				(match regs.(a), regs.(b) with
				| VFloat a, VFloat b -> VFloat (fop a b)
				| _ -> assert false)
			| _ ->
				assert false
		in
		let iop f a b =
			match rtype a with
			(* todo : sign-extend and mask after result for HI8/16 *)
			| HI8 | HI16 | HI32 ->
				(match regs.(a), regs.(b) with
				| VInt a, VInt b -> VInt (f a b)
				| _ -> assert false)
			| _ ->
				assert false
		in
		let iunop iop r =
			match rtype r with
			| HI8 | HI16 | HI32 ->
				(match regs.(r) with
				| VInt a -> VInt (iop a)
				| _ -> assert false)
			| _ ->
				assert false
		in
		let ucompare a b =
			match a, b with
			| VInt a, VInt b ->
				let d = Int32.sub (Int32.shift_right_logical a 16) (Int32.shift_right_logical b 16) in
				Int32.to_int (if d = 0l then Int32.sub (Int32.logand a 0xFFFFl) (Int32.logand b 0xFFFFl) else d)
			| _ -> assert false
		in
		let vcompare ra rb =
			let a = get ra in
			let b = get rb in
			match a, b with
			| VInt a, VInt b -> Int32.compare a b
			| VFloat a, VFloat b -> compare a b
			| VNull, VNull -> 0
			| VNull, _ -> 1
			| _, VNull -> -1
			| VObj oa, VObj ob ->
				if oa == ob then 0 else
				let fid = ref None in
				Array.iter (fun p -> if p.fname = "__compare" then fid := Some p.fmethod) oa.oproto.pclass.pproto;
				(match !fid with
				| None -> 1
				| Some f -> (match fcall (func f) [a;b] with VInt i -> Int32.to_int i | _ -> assert false));
			| _ ->
				error ("Can't compare " ^ vstr_d a ^ " and " ^ vstr_d b)
		in
		let set_i32 b p v =
			String.set b p (char_of_int ((Int32.to_int v) land 0xFF));
			String.set b (p+1) (char_of_int ((Int32.to_int (Int32.shift_right_logical v 8)) land 0xFF));
			String.set b (p+2) (char_of_int ((Int32.to_int (Int32.shift_right_logical v 16)) land 0xFF));
			String.set b (p+3) (char_of_int (Int32.to_int (Int32.shift_right_logical v 24)));
		in
		let get_i32 b p =
			let i = int_of_char (String.get b p) in
			let j = int_of_char (String.get b (p + 1)) in
			let k = int_of_char (String.get b (p + 2)) in
			let l = int_of_char (String.get b (p + 3)) in
			Int32.logor (Int32.of_int (i lor (j lsl 8) lor (k lsl 16))) (Int32.shift_left (Int32.of_int l) 24);
		in
		let rec loop() =
			let op = f.code.(!pos) in
			incr pos;
			(match op with
			| OMov (a,b) -> set a (get b)
			| OInt (r,i) -> set r (VInt code.ints.(i))
			| OFloat (r,i) -> set r (VFloat (Array.unsafe_get code.floats i))
			| OString (r,s) -> set r (VBytes (code.strings.(s) ^ "\x00"))
			| OBool (r,b) -> set r (VBool b)
			| ONull r -> set r VNull
			| OAdd (r,a,b) -> set r (numop Int32.add ( +. ) a b)
			| OSub (r,a,b) -> set r (numop Int32.sub ( -. ) a b)
			| OMul (r,a,b) -> set r (numop Int32.mul ( *. ) a b)
			| OSDiv (r,a,b) -> set r (numop (fun a b -> if b = 0l then 0l else Int32.div a b) ( /. ) a b)
			| OUDiv (r,a,b) -> set r (iop (fun a b -> if b = 0l then 0l else assert false (* TODO : unsigned div *)) a b)
			| OSMod (r,a,b) -> set r (numop (fun a b -> if b = 0l then 0l else Int32.rem a b) mod_float a b)
			| OUMod (r,a,b) -> set r (iop (fun a b -> if b = 0l then 0l else assert false (* TODO : unsigned mod *)) a b)
			| OShl (r,a,b) -> set r (iop (fun a b -> Int32.shift_left a (Int32.to_int b)) a b)
			| OSShr (r,a,b) -> set r (iop (fun a b -> Int32.shift_right a (Int32.to_int b)) a b)
			| OUShr (r,a,b) -> set r (iop (fun a b -> Int32.shift_right_logical a (Int32.to_int b)) a b)
			| OAnd (r,a,b) -> set r (iop Int32.logand a b)
			| OOr (r,a,b) -> set r (iop Int32.logor a b)
			| OXor (r,a,b) -> set r (iop Int32.logxor a b)
			| ONeg (r,v) -> set r (match get v with VInt v -> VInt (Int32.neg v) | VFloat f -> VFloat (-. f) | _ -> assert false)
			| ONot (r,v) -> set r (match get v with VBool b -> VBool (not b) | _ -> assert false)
			| OIncr r -> set r (iunop (fun i -> Int32.add i 1l) r)
			| ODecr r -> set r (iunop (fun i -> Int32.sub i 1l) r)
			| OCall0 (r,f) -> set r (fcall (func f) [])
			| OCall1 (r,f,r1) -> set r (fcall (func f) [get r1])
			| OCall2 (r,f,r1,r2) -> set r (fcall (func f) [get r1;get r2])
			| OCall3 (r,f,r1,r2,r3) -> set r (fcall (func f) [get r1;get r2;get r3])
			| OCall4 (r,f,r1,r2,r3,r4) -> set r (fcall (func f) [get r1;get r2;get r3;get r4])
			| OCallN (r,f,rl) -> set r (fcall (func f) (List.map get rl))
			| OGetGlobal (r,g) -> set r (global g)
			| OSetGlobal (g,r) ->
				let v = get r in
				check v code.globals.(g) (fun() -> "global " ^ string_of_int g);
				Array.unsafe_set globals g v
			| OEq (r,a,b) -> set r (VBool (vcompare a b = 0))
			| ONotEq (r,a,b) -> set r (VBool (vcompare a b <> 0))
			| OSGte (r,a,b) -> set r (VBool (vcompare a b >= 0))
			| OSLt (r,a,b) -> set r (VBool (vcompare a b < 0))
			| OUGte (r,a,b) -> set r (VBool (ucompare (get a) (get b) >= 0))
			| OULt (r,a,b) -> set r (VBool (ucompare (get a) (get b) < 0))
			| OJTrue (r,i) -> if get r = VBool true then pos := !pos + i
			| OJFalse (r,i) -> if get r = VBool false then pos := !pos + i
			| ORet r -> raise (Return regs.(r))
			| OJNull (r,i) -> if get r == VNull then pos := !pos + i
			| OJNotNull (r,i) -> if get r != VNull then pos := !pos + i
			| OJSLt (a,b,i) -> if vcompare a b < 0 then pos := !pos + i
			| OJSGte (a,b,i) -> if vcompare a b >= 0 then pos := !pos + i
			| OJULt (a,b,i) -> if ucompare (get a) (get b) < 0 then pos := !pos + i
			| OJUGte (a,b,i) -> if ucompare (get a) (get b) >= 0 then pos := !pos + i
			| OJEq (a,b,i) -> if vcompare a b = 0 then pos := !pos + i
			| OJNeq (a,b,i) -> if vcompare a b <> 0 then pos := !pos + i
			| OJAlways i -> pos := !pos + i
			| OToDyn (r,a) -> set r (VDyn (get a, f.regs.(a)))
			| OToFloat (r,a) -> set r (match get a with VInt v -> VFloat (Int32.to_float v) | _ -> assert false)
			| OToInt (r,a) -> set r (match get a with VFloat v -> VInt (Int32.of_float v) | _ -> assert false)
			| OLabel _ -> ()
			| ONew r ->
				set r (match rtype r with
				| HDynObj -> VDynObj { dfields = Hashtbl.create 0; dvalues = [||]; dtypes = [||]; dvirtuals = []; }
				| HObj p ->
					let p, fields = get_proto p in
					VObj { oproto = p; ofields = Array.map default fields }
				| _ -> assert false)
			| OField (r,o,fid) ->
				set r (match get o with
					| VObj v -> v.ofields.(fid)
					| VVirtual v -> (match v.vindexes.(fid) with VFNone -> VNull | VFIndex i -> v.vtable.(i))
					| VNull -> error "Null access"
					| _ -> assert false)
			| OSetField (o,fid,r) ->
				let rv = get r in
				let o = get o in
				(match o with
				| VObj v ->
					check_obj rv o fid;
					v.ofields.(fid) <- rv
				| VVirtual v ->
					(match v.vindexes.(fid) with
					| VFNone -> assert false (* TODO *)
					| VFIndex i ->
						check_obj rv o fid;
						v.vtable.(i) <- rv)
				| VNull -> error "Null access"
				| _ -> assert false)
			| OGetThis (r, fid) ->
				set r (match get 0 with VObj v -> v.ofields.(fid) | _ -> assert false)
			| OSetThis (fid, r) ->
				(match get 0 with
				| VObj v as o ->
					let rv = get r in
					check_obj rv o fid;
					v.ofields.(fid) <- rv
				| _ -> assert false)
			| OCallMethod (r,m,rl) ->
				(match get (List.hd rl) with
				| VObj v -> set r (fcall v.oproto.pmethods.(m) (List.map get rl))
				| VNull -> error "Null access"
				| _ -> assert false)
			| OCallThis (r,m,rl) ->
				(match get 0 with
				| VObj v as o -> set r (fcall v.oproto.pmethods.(m) (o :: List.map get rl))
				| _ -> assert false)
			| OCallClosure (r,v,rl) ->
				(match get v with
				| VClosure (f,None) -> set r (fcall f (List.map get rl))
				| VClosure (f,Some arg) -> set r (fcall f (arg :: List.map get rl))
				| VNull -> error "Null function"
				| _ -> assert false)
			| OGetFunction (r, fid) ->
				let f = functions.(fid) in
				set r (VClosure (f,None))
			| OClosure (r, fid, v) ->
				let f = functions.(fid) in
				set r (VClosure (f,Some (get v)))
			| OMethod (r, o, m) ->
				set r (match get o with
				| VObj v as obj -> VClosure (v.oproto.pmethods.(m), Some obj)
				| VNull -> error "Null access"
				| VVirtual v ->
					let name, _, _ = v.vtype.vfields.(m) in
					(match v.vvalue with
					| VObj o as obj ->
						let m = (try PMap.find name o.oproto.pclass.pfunctions with Not_found -> assert false) in
						VClosure (functions.(m), Some obj)
					| _ -> assert false)
				| _ -> assert false)
			| OThrow r ->
				throw (get r)
			| OGetI8 (r,b,p) ->
				(match get b, get p with
				| VBytes b, VInt p -> set r (VInt (Int32.of_int (int_of_char (String.get b (Int32.to_int p)))))
				| _ -> assert false)
			| OGetI32 (r,b,p) ->
				(match get b, get p with
				| VBytes b, VInt p -> set r (VInt (get_i32 b (Int32.to_int p)))
				| _ -> assert false)
			| OGetF32 (r,b,p) ->
				(match get b, get p with
				| VBytes b, VInt p -> set r (VFloat (Int32.float_of_bits (get_i32 b (Int32.to_int p))))
				| _ -> assert false)
			| OGetF64 (r,b,p) ->
				(match get b, get p with
				| VBytes b, VInt p ->
					let p = Int32.to_int p in
					let i64 = Int64.logor (Int64.of_int32 (get_i32 b p)) (Int64.shift_left (Int64.of_int32 (get_i32 b (p + 4))) 32) in
					set r (VFloat (Int64.float_of_bits i64))
				| _ -> assert false)
			| OGetArray (r,a,i) ->
				(match get a, get i with
				| VArray (a,_), VInt i -> set r a.(Int32.to_int i)
				| _ -> assert false);
			| OSetI8 (r,p,v) ->
				(match get r, get p, get v with
				| VBytes b, VInt p, VInt v -> String.set b (Int32.to_int p) (char_of_int ((Int32.to_int v) land 0xFF))
				| _ -> assert false)
			| OSetI32 (r,p,v) ->
				(match get r, get p, get v with
				| VBytes b, VInt p, VInt v -> set_i32 b (Int32.to_int p) v
				| _ -> assert false)
			| OSetF32 (r,p,v) ->
				(match get r, get p, get v with
				| VBytes b, VInt p, VFloat v -> set_i32 b (Int32.to_int p) (Int32.bits_of_float v)
				| _ -> assert false)
			| OSetF64 (r,p,v) ->
				(match get r, get p, get v with
				| VBytes b, VInt p, VFloat v ->
					let p = Int32.to_int p in
					let v64 = Int64.bits_of_float v in
					set_i32 b p (Int64.to_int32 v64);
					set_i32 b (p + 4) (Int64.to_int32 (Int64.shift_right_logical v64 32));
				| _ -> assert false)
			| OSetArray (a,i,v) ->
				(match get a, get i with
				| VArray (a,t), VInt i ->
					let v = get v in
					check v t (fun() -> "array");
					a.(Int32.to_int i) <- v
				| _ -> assert false);
			| OSafeCast (r, v) ->
				let v = get v in
				set r (match v, rtype r with
					| VObj o, HObj c when o.oproto.pclass == c -> v
					| VNull, t -> default t
					| VDyn (v, t1), t2 when t1 == t2 -> v
					| _, t -> error ("Failed to cast " ^ vstr_d v ^ " to " ^ tstr t)
				)
			| OUnsafeCast (r,v) ->
				set r (get v)
			| OArraySize (r,a) ->
				(match get a with
				| VArray (a,_) -> set r (VInt (Int32.of_int (Array.length a)));
				| _ -> assert false)
			| OError s ->
				throw (VDyn (VBytes (code.strings.(s) ^ "\x00"),HBytes))
			| OType (r,t) ->
				set r (VType t)
			| ORef (r,v) ->
				set r (VRef (regs,v,rtype v))
			| OUnref (v,r) ->
				set v (match get r with
				| VRef (regs,i,_) -> Array.unsafe_get regs i
				| _ -> assert false)
			| OSetref (r,v) ->
				(match get r with
				| VRef (regs,i,t) ->
					let v = get v in
					check v t (fun() -> "ref");
					Array.unsafe_set regs i v
				| _ -> assert false)
			| OToVirtual (r,rv) ->
				let v = get rv in
				set r (match v, rtype r with
				| VNull, _ -> VNull
				| VObj o, HVirtual vp ->
					let indexes = Array.mapi (fun i (n,_,t) ->
						try
							let idx, ft = PMap.find n o.oproto.pclass.pindex in
							if not (tsame t ft) then error ("Can't cast " ^ tstr (rtype rv) ^ " to " ^ tstr (rtype r) ^ "(" ^ n ^ " type differ)");
							VFIndex idx
						with Not_found ->
							VFNone (* most likely a method *)
					) vp.vfields in
					let v = {
						vtype = vp;
						vindexes = indexes;
						vtable = o.ofields;
						vvalue = v;
					} in
					VVirtual v
				| VDynObj d, HVirtual vp ->
					(try
						VVirtual (List.find (fun v -> v.vtype == vp) d.dvirtuals)
					with Not_found ->
						let indexes = Array.mapi (fun i (n,_,t) ->
							try
								let idx = Hashtbl.find d.dfields n in
								if not (tsame t d.dtypes.(idx)) then error ("Can't cast " ^ tstr (rtype rv) ^ " to " ^ tstr (rtype r) ^ "(" ^ n ^ " type differ)");
								VFIndex idx
							with Not_found ->
								VFNone
						) vp.vfields in
						let v = {
							vtype = vp;
							vindexes = indexes;
							vtable = d.dvalues;
							vvalue = v;
						} in
						d.dvirtuals <- v :: d.dvirtuals;
						VVirtual v
					)
				| _ -> assert false)
			| OUnVirtual (r,v) ->
				set r (match get v with VNull -> VNull | VVirtual v -> v.vvalue | _ -> assert false)
			| ODynGet (r,o,f) ->
				let obj = (match get o with VVirtual v -> v.vvalue | v -> v) in
				let set_with v t =
					if tsame t (rtype r) then
						set r v
					else match t, rtype r with
					| (HI8|HI16|HI32), (HF32|HF64) ->
						set r (match v with VInt i -> VFloat (Int32.to_float i) | _ -> assert false)
					| _, HDyn None ->
						set r (if safe_cast t (HDyn None) then v else VDyn (v,t))
					| _ ->
						error ("Can't cast " ^ tstr t ^ " to " ^ tstr (rtype r))
				in
				(match obj with
				| VDynObj d ->
					(try
						let idx = Hashtbl.find d.dfields code.strings.(f) in
						set_with d.dvalues.(idx) d.dtypes.(idx)
					with Not_found ->
						set r (default (rtype r)))
				| VObj o ->
					(try
						let idx, t = PMap.find code.strings.(f) o.oproto.pclass.pindex in
						set_with o.ofields.(idx) t
					with Not_found ->
						set r (default (rtype r)))
				| _ ->
					assert false)
			| ODynSet (o,fid,vr) ->
				let obj = (match get o with VVirtual v -> v.vvalue | v -> v) in
				let v = get vr in
				check_obj v obj fid;
				(match obj with
				| VDynObj d ->
					let rebuild_virtuals() =
						if d.dvirtuals <> [] then assert false (* TODO : update virtuals table *)
					in
					let v, vt = (match rtype vr with
						| HDyn _ ->
							(match v with
							| VDyn (v,t) -> v,t
							| VObj o -> v, HObj o.oproto.pclass
							| VDynObj _ -> v, HDynObj
							| VVirtual vp -> v, HVirtual vp.vtype
							| _ -> assert false)
						| t -> v, t
					) in
					(try
						let idx = Hashtbl.find d.dfields code.strings.(fid) in
						d.dvalues.(idx) <- v;
						if not (tsame d.dtypes.(idx) vt) then begin
							d.dtypes.(idx) <- vt;
							rebuild_virtuals();
						end;
					with Not_found ->
						let idx = Array.length d.dvalues in
						Hashtbl.add d.dfields code.strings.(fid) idx;
						let vals2 = Array.make (idx + 1) VNull in
						let types2 = Array.make (idx + 1) HVoid in
						Array.blit d.dvalues 0 vals2 0 idx;
						Array.blit d.dtypes 0 types2 0 idx;
						vals2.(idx) <- v;
						types2.(idx) <- vt;
						d.dvalues <- vals2;
						d.dtypes <- types2;
						rebuild_virtuals();
					)
				| _ ->
					assert false)
			| OMakeEnum (r,e,pl) ->
				set r (VEnum (e,Array.map get (Array.of_list pl)))
			| OEnumAlloc (r,f) ->
				(match rtype r with
				| HEnum e ->
					let _, _, fl = e.efields.(f) in
					let vl = Array.create (Array.length fl) VUndef in
					set r (VEnum (f, vl))
				| _ -> assert false
				)
			| OEnumIndex (r,v) ->
				(match get v with
				| VEnum (i,_) -> set r (VInt (Int32.of_int i))
				| _ -> assert false)
			| OEnumField (r, v, _, i) ->
				(match get v with
				| VEnum (_,vl) -> set r vl.(i)
				| _ -> assert false)
			| OSetEnumField (v, i, r) ->
				(match get v, rtype v with
				| VEnum (id,vl), HEnum e ->
					let rv = get r in
					let _, _, fields = e.efields.(id) in
					check rv fields.(i) (fun() -> "enumfield");
					vl.(i) <- rv
				| _ -> assert false)
			| OSwitch (r, indexes) ->
				(match get r with
				| VInt i ->
					let i = Int32.to_int i in
					if i >= 0 && i < Array.length indexes then pos := !pos + indexes.(i)
				| _ -> assert false)
			| ONullCheck r ->
				if get r = VNull then error "Null access"
			| OTrap (r,j) ->
				let target = !pos + j in
				traps := (r,target) :: !traps
			| OEndTrap _ ->
				traps := List.tl !traps
			| ODump r ->
				print_endline (vstr_d (get r));
			);
			loop()
		in
		let rec exec() =
			try
				loop()
			with
				| Return v -> stack := List.tl !stack; v
				| InterpThrow v ->
					match !traps with
					| [] ->
						exc_stack := List.hd !stack :: !exc_stack;
						stack := List.tl !stack;
						raise (InterpThrow v)
					| (r,target) :: tl ->
						traps := tl;
						pos := target;
						set r v;
						exec()
		in
		exec()
	in
	let int = Int32.to_int in
	let load_native lib name =
		FNativeFun (lib ^ "@" ^ name, (match lib with
		| "std" ->
			(match name with
			| "balloc" ->
				(function
				| [VInt i] -> VBytes (String.create (int i))
				| _ -> assert false)
			| "aalloc" ->
				(function
				| [VType t;VInt i] -> VArray (Array.create (int i) VNull,t)
				| _ -> assert false)
			| "ablit" ->
				(function
				| [VArray (dst,_); VInt dp; VArray (src,_); VInt sp; VInt len] ->
					Array.blit src (int sp) dst (int dp) (int len);
					VUndef
				| _ -> assert false)
			| "bblit" ->
				(function
				| [VBytes dst; VInt dp; VBytes src; VInt sp; VInt len] ->
					String.blit src (int sp) dst (int dp) (int len);
					VUndef
				| _ -> assert false)
			| "itos" ->
				(function
				| [VInt v; VRef (regs,i,_)] ->
					let str = Int32.to_string v in
					regs.(i) <- VInt (Int32.of_int (String.length str));
					VBytes (str ^ "\x00")
				| _ -> assert false);
			| "ftos" ->
				(function
				| [VFloat v; VRef (regs,i,_)] ->
					let str = string_of_float v in
					regs.(i) <- VInt (Int32.of_int (String.length str));
					VBytes (str ^ "\x00")
				| _ -> assert false);
			| "value_to_string" ->
				(function
				| [v; VRef (regs,i,_)] ->
					let str = vstr v (HDyn None) in
					regs.(i) <- VInt (Int32.of_int (String.length str));
					VBytes (str ^ "\x00")
				| _ -> assert false);
			| "utf8length" ->
				(function
				| [VBytes b; VInt start; VInt len] ->
					VInt (Int32.of_int (UTF8.length (String.sub b (int start) (int len))))
				| _ -> assert false)
			| "utf8char" ->
				(function
				| [VBytes b; VInt start; VInt len; VInt index] ->
					VInt (Int32.of_int (try UChar.code (UTF8.get (String.sub b (int start) (int len)) (int index)) with _ -> 0))
				| _ -> assert false)
			| "math_sqrt" ->
				(function
				| [VFloat f] -> VFloat (sqrt f)
				| _ -> assert false)
			| "parse_int" ->
				(function
				| [VBytes str; VInt len] ->
					(try
						let i = (match Interp.parse_int (String.sub str 0 (int len)) with
							| Interp.VInt v -> Int32.of_int v
							| Interp.VInt32 v -> v
							| _ -> assert false
						) in
						VInt i
					with _ ->
						VNull)
				| _ -> assert false)
			| "parse_float" ->
				(function
				| [VBytes str; VInt len] -> (try VFloat (Interp.parse_float (String.sub str 0 (int len))) with _ -> VFloat nan)
				| _ -> assert false)
			| "bytes_compare" ->
				(function
				| [VBytes a; VInt apos; VBytes b; VInt bpos; VInt len] -> VInt (Int32.of_int (String.compare (String.sub a (int apos) (int len)) (String.sub b (int bpos) (int len))))
				| _ -> assert false)
			| "hballoc" ->
				(function
				| [] -> VAbstract (AHashBytes (Hashtbl.create 0))
				| _ -> assert false)
			| "sys_print" ->
				(function
				| [VBytes str] -> print_string (try String.sub str 0 (String.index str '\000') with Not_found -> str); VUndef
				| _ -> assert false)
			| "sys_exit" ->
				(function
				| [VInt code] -> VUndef
				| _ -> assert false)
			| _ -> (fun args -> error ("Unresolved native " ^ name)))
		| _ ->
			(fun args -> error ("Unresolved native " ^ name))))
	in
	Array.iter (fun (lib,name,_,idx) -> functions.(idx) <- load_native code.strings.(lib) code.strings.(name)) code.natives;
	Array.iter (fun fd -> functions.(fd.findex) <- FFun fd) code.functions;
	let get_stack st =
		String.concat "\n" (List.map (fun (f,pos) ->
			let pos = !pos - 1 in
			let file, line = (try let fid, line = f.debug.(pos) in code.debugfiles.(fid), line with _ -> "???", 0) in
			Printf.sprintf "Called from fun(%d)@%d (%s line %d)" f.findex pos file line
		) st)
	in
	match functions.(code.entrypoint) with
	| FFun f when f.ftype = HFun([],HVoid) ->
		(try
			ignore(call f [])
		with
			| InterpThrow v -> Common.error ("Uncaught exception " ^ vstr_d v ^ "\n" ^ get_stack (List.rev !exc_stack)) Ast.null_pos
			| Runtime_error msg -> Common.error ("HL Interp error " ^ msg ^ "\n" ^ get_stack !stack) Ast.null_pos
		)
	| _ -> assert false

(* --------------------------------------------------------------------------------------------------------------------- *)
(* WRITE *)


(* 	from -500M to +500M
	0[7] = 0-127
	10[+/-][5] [8] = -x2000/+x2000
	11[+/-][5] [24] = -x20000000/+x20000000
*)
let write_index_gen b i =
	if i < 0 then
		let i = -i in
		if i < 0x2000 then begin
			b ((i lsr 8) lor 0xA0);
			b (i land 0xFF);
		end else if i >= 0x20000000 then assert false else begin
			b ((i lsr 24) lor 0xE0);
			b ((i lsr 16) land 0xFF);
			b ((i lsr 8) land 0xFF);
			b (i land 0xFF);
		end
	else if i < 0x80 then
		b i
	else if i < 0x2000 then begin
		b ((i lsr 8) lor 0x80);
		b (i land 0xFF);
	end else if i >= 0x20000000 then assert false else begin
		b ((i lsr 24) lor 0xC0);
		b ((i lsr 16) land 0xFF);
		b ((i lsr 8) land 0xFF);
		b (i land 0xFF);
	end

let write_code ch code =

	let types = new_lookup() in
	let byte = IO.write_byte ch in
	let write_index = write_index_gen byte in

	let rec write_type t =
		write_index (lookup types t (fun() -> assert false))
	in

	let write_op op =

		let o = Obj.repr op in
		let oid = Obj.tag o in

		match op with
		| OLabel _ | OEndTrap _ ->
			byte oid
		| OCall2 (r,g,a,b) ->
			byte oid;
			write_index r;
			write_index g;
			write_index a;
			write_index b;
		| OCall3 (r,g,a,b,c) ->
			byte oid;
			write_index r;
			write_index g;
			write_index a;
			write_index b;
			write_index c;
		| OCall4 (r,g,a,b,c,d) ->
			byte oid;
			write_index r;
			write_index g;
			write_index a;
			write_index b;
			write_index c;
			write_index d;
		| OCallN (r,f,rl) | OCallClosure (r,f,rl) | OCallMethod (r,f,rl) | OCallThis (r,f,rl) | OMakeEnum (r,f,rl) ->
			byte oid;
			write_index r;
			write_index f;
			let n = List.length rl in
			if n > 0xFF then assert false;
			byte n;
			List.iter write_index rl
		| OType (r,t) ->
			byte oid;
			write_index r;
			write_type t
		| OSwitch (r,pl) ->
			byte oid;
			let n = Array.length pl in
			if n > 0xFF then assert false;
			byte n;
			Array.iter write_index pl
		| OEnumField (r,e,i,idx) ->
			byte oid;
			write_index r;
			write_index e;
			write_index i;
			write_index idx;
		| _ ->
			let field n = (Obj.magic (Obj.field o n) : int) in
			match Obj.size o with
			| 1 ->
				let a = field 0 in
				byte oid;
				write_index a;
			| 2 ->
				let a = field 0 in
				let b = field 1 in
				byte oid;
				write_index a;
				write_index b;
			| 3 ->
				let a = field 0 in
				let b = field 1 in
				let c = field 2 in
				byte oid;
				write_index a;
				write_index b;
				write_index c;
			| _ ->
				assert false
	in

	IO.nwrite ch "HLB";
	IO.write_byte ch code.version;

	let rec get_type t =
		ignore(lookup types t (fun() ->
			(match t with
			| HFun (args, ret) ->
				List.iter get_type args;
				get_type ret
			| HObj p ->
				(match p.psuper with None -> () | Some p -> get_type (HObj p));
				Array.iter (fun (_,n,t) -> get_type t) p.pfields
			| HDyn (Some t) | HRef t ->
				get_type t
			| HVirtual v ->
				Array.iter (fun (_,_,t) -> get_type t) v.vfields
			| HEnum e ->
				Array.iter (fun (_,_,tl) -> Array.iter get_type tl) e.efields
			| _ ->
				());
			t
		));
	in
	List.iter (fun t -> get_type t) [HVoid; HI8; HI16; HI32; HF32; HF64; HBool; HType; HDyn None]; (* make sure all basic types get lower indexes *)
	Array.iter (fun g -> get_type g) code.globals;
	Array.iter (fun (_,_,t,_) -> get_type t) code.natives;
	Array.iter (fun f -> get_type f.ftype; Array.iter (fun r -> get_type r) f.regs) code.functions;

	write_index (Array.length code.ints);
	write_index (Array.length code.floats);
	write_index (Array.length code.strings);
	write_index (DynArray.length types.arr);
	write_index (Array.length code.globals);
	write_index (Array.length code.natives);
	write_index (Array.length code.functions);
	write_index code.entrypoint;

	Array.iter (IO.write_real_i32 ch) code.ints;
	Array.iter (IO.write_double ch) code.floats;

	let str_length = ref 0 in
	Array.iter (fun str -> str_length := !str_length + String.length str + 1) code.strings;
	IO.write_i32 ch !str_length;
	Array.iter (IO.write_string ch) code.strings;
	Array.iter (fun str -> write_index (String.length str)) code.strings;

	DynArray.iter (fun t ->
		match t with
		| HVoid -> byte 0
		| HI8 -> byte 1
		| HI16 -> byte 2
		| HI32 -> byte 3
		| HF32 -> byte 4
		| HF64 -> byte 5
		| HBool -> byte 6
		| HBytes -> byte 7
		| HDyn None -> byte 8
		| HDyn (Some t) ->
			byte 0x88;
			write_type t
		| HFun (args,ret) ->
			let n = List.length args in
			if n > 0xFF then assert false;
			byte 9;
			byte n;
			List.iter write_type args;
			write_type ret
		| HObj p ->
			byte 10;
			write_index p.pid;
			(match p.psuper with
			| None -> write_index (-1)
			| Some t -> write_type (HObj t));
			write_index (Array.length p.pfields);
			write_index (Array.length p.pproto);
			Array.iter (fun (_,n,t) -> write_index n; write_type t) p.pfields;
			Array.iter (fun f -> write_index f.fid; write_index f.fmethod; write_index (match f.fvirtual with None -> -1 | Some i -> i)) p.pproto;
		| HArray ->
			byte 11
		| HType ->
			byte 12
		| HRef t ->
			byte 13;
			write_type t
		| HVirtual v ->
			byte 14;
			write_index (Array.length v.vfields);
			Array.iter (fun (_,sid,t) -> write_index sid; write_type t) v.vfields
		| HDynObj ->
			byte 15
		| HAbstract (_,i) ->
			byte 16;
			write_index i
		| HEnum e ->
			byte 17;
			write_index e.eid;
			write_index (Array.length e.efields);
			Array.iter (fun (_,n,tl) ->
				write_index (Array.length tl);
				Array.iter write_type tl;
			) e.efields
	) types.arr;

	Array.iter write_type code.globals;
	Array.iter (fun (lib_index, name_index,ttype,findex) ->
		write_index lib_index;
		write_index name_index;
		write_type ttype;
		write_index findex;
	) code.natives;
	Array.iter (fun f ->
		write_type f.ftype;
		write_index f.findex;
		write_index (Array.length f.regs);
		write_index (Array.length f.code);
		Array.iter write_type f.regs;
		Array.iter write_op f.code;
	) code.functions

(* --------------------------------------------------------------------------------------------------------------------- *)
(* DUMP *)

let ostr o =
	match o with
	| OMov (a,b) -> Printf.sprintf "mov %d,%d" a b
	| OInt (r,i) -> Printf.sprintf "int %d,@%d" r i
	| OFloat (r,i) -> Printf.sprintf "float %d,@%d" r i
	| OString (r,s) -> Printf.sprintf "string %d,@%d" r s
	| OBool (r,b) -> if b then Printf.sprintf "true %d" r else Printf.sprintf "false %d" r
	| ONull r -> Printf.sprintf "null %d" r
	| OAdd (r,a,b) -> Printf.sprintf "add %d,%d,%d" r a b
	| OSub (r,a,b) -> Printf.sprintf "sub %d,%d,%d" r a b
	| OMul (r,a,b) -> Printf.sprintf "mul %d,%d,%d" r a b
	| OSDiv (r,a,b) -> Printf.sprintf "sdiv %d,%d,%d" r a b
	| OUDiv (r,a,b) -> Printf.sprintf "udiv %d,%d,%d" r a b
	| OSMod (r,a,b) -> Printf.sprintf "smod %d,%d,%d" r a b
	| OUMod (r,a,b) -> Printf.sprintf "umod %d,%d,%d" r a b
	| OShl (r,a,b) -> Printf.sprintf "shl %d,%d,%d" r a b
	| OSShr (r,a,b) -> Printf.sprintf "sshr %d,%d,%d" r a b
	| OUShr (r,a,b) -> Printf.sprintf "ushr %d,%d,%d" r a b
	| OAnd (r,a,b) -> Printf.sprintf "and %d,%d,%d" r a b
	| OOr (r,a,b) -> Printf.sprintf "or %d,%d,%d" r a b
	| OXor (r,a,b) -> Printf.sprintf "xor %d,%d,%d" r a b
	| ONeg (r,v) -> Printf.sprintf "neg %d,%d" r v
	| ONot (r,v) -> Printf.sprintf "not %d,%d" r v
	| OIncr r -> Printf.sprintf "incr %d" r
	| ODecr r -> Printf.sprintf "decr %d" r
	| OCall0 (r,g) -> Printf.sprintf "call %d, f%d()" r g
	| OCall1 (r,g,a) -> Printf.sprintf "call %d, f%d(%d)" r g a
	| OCall2 (r,g,a,b) -> Printf.sprintf "call %d, f%d(%d,%d)" r g a b
	| OCall3 (r,g,a,b,c) -> Printf.sprintf "call %d, f%d(%d,%d,%d)" r g a b c
	| OCall4 (r,g,a,b,c,d) -> Printf.sprintf "call %d, f%d(%d,%d,%d,%d)" r g a b c d
	| OCallN (r,g,rl) -> Printf.sprintf "call %d, f%d(%s)" r g (String.concat "," (List.map string_of_int rl))
	| OCallMethod (r,f,[]) -> "callmethod ???"
	| OCallMethod (r,f,o :: rl) -> Printf.sprintf "callmethod %d, %d[%d](%s)" r o f (String.concat "," (List.map string_of_int rl))
	| OCallClosure (r,f,rl) -> Printf.sprintf "callclosure %d, %d(%s)" r f (String.concat "," (List.map string_of_int rl))
	| OCallThis (r,f,rl) -> Printf.sprintf "callthis %d, [%d](%s)" r f (String.concat "," (List.map string_of_int rl))
	| OGetFunction (r,f) -> Printf.sprintf "getfunction %d, f%d" r f
	| OClosure (r,f,v) -> Printf.sprintf "closure %d, f%d(%d)" r f v
	| OGetGlobal (r,g) -> Printf.sprintf "global %d, %d" r g
	| OSetGlobal (g,r) -> Printf.sprintf "setglobal %d, %d" g r
	| OEq (r,a,b) -> Printf.sprintf "eq %d,%d,%d" r a b
	| ONotEq (r,a,b)  -> Printf.sprintf "noteq %d,%d,%d" r a b
	| OSLt (r,a,b) -> Printf.sprintf "slt %d,%d,%d" r a b
	| OSGte (r,a,b) -> Printf.sprintf "sgte %d,%d,%d" r a b
	| OULt (r,a,b) -> Printf.sprintf "ult %d,%d,%d" r a b
	| OUGte (r,a,b) -> Printf.sprintf "ugte %d,%d,%d" r a b
	| ORet r -> Printf.sprintf "ret %d" r
	| OJTrue (r,d) -> Printf.sprintf "jtrue %d,%d" r d
	| OJFalse (r,d) -> Printf.sprintf "jfalse %d,%d" r d
	| OJNull (r,d) -> Printf.sprintf "jnull %d,%d" r d
	| OJNotNull (r,d) -> Printf.sprintf "jnotnull %d,%d" r d
	| OJSLt (a,b,i) -> Printf.sprintf "jslt %d,%d,%d" a b i
	| OJSGte (a,b,i) -> Printf.sprintf "jsgte %d,%d,%d" a b i
	| OJULt (a,b,i) -> Printf.sprintf "jult %d,%d,%d" a b i
	| OJUGte (a,b,i) -> Printf.sprintf "jugte %d,%d,%d" a b i
	| OJEq (a,b,i) -> Printf.sprintf "jeq %d,%d,%d" a b i
	| OJNeq (a,b,i) -> Printf.sprintf "jneq %d,%d,%d" a b i
	| OJAlways d -> Printf.sprintf "jalways %d" d
	| OToDyn (r,a) -> Printf.sprintf "todyn %d,%d" r a
	| OToFloat (r,a) -> Printf.sprintf "tofloat %d,%d" r a
	| OToInt (r,a) -> Printf.sprintf "toint %d,%d" r a
	| OLabel _ -> "label"
	| ONew r -> Printf.sprintf "new %d" r
	| OField (r,o,i) -> Printf.sprintf "field %d,%d[%d]" r o i
	| OMethod (r,o,m) -> Printf.sprintf "method %d,%d[%d]" r o m
	| OSetField (o,i,r) -> Printf.sprintf "setfield %d[%d],%d" o i r
	| OGetThis (r,i) -> Printf.sprintf "getthis %d,[%d]" r i
	| OSetThis (i,r) -> Printf.sprintf "setthis [%d],%d" i r
	| OThrow r -> Printf.sprintf "throw %d" r
	| OGetI8 (r,b,p) -> Printf.sprintf "geti8 %d,%d[%d]" r b p
	| OGetI32 (r,b,p) -> Printf.sprintf "geti32 %d,%d[%d]" r b p
	| OGetF32 (r,b,p) -> Printf.sprintf "getf32 %d,%d[%d]" r b p
	| OGetF64 (r,b,p) -> Printf.sprintf "getf64 %d,%d[%d]" r b p
	| OGetArray (r,a,i) -> Printf.sprintf "getarray %d,%d[%d]" r a i
	| OSetI8 (r,p,v) -> Printf.sprintf "seti8 %d,%d,%d" r p v
	| OSetI32 (r,p,v) -> Printf.sprintf "seti32 %d,%d,%d" r p v
	| OSetF32 (r,p,v) -> Printf.sprintf "setf32 %d,%d,%d" r p v
	| OSetF64 (r,p,v) -> Printf.sprintf "setf64 %d,%d,%d" r p v
	| OSetArray (a,i,v) -> Printf.sprintf "setarray %d[%d],%d" a i v
	| OSafeCast (r,v) -> Printf.sprintf "safecast %d,%d" r v
	| OUnsafeCast (r,v) -> Printf.sprintf "unsafecast %d,%d" r v
	| OArraySize (r,a) -> Printf.sprintf "arraysize %d,%d" r a
	| OError s -> Printf.sprintf "error @%d" s
	| OType (r,t) -> Printf.sprintf "type %d,%s" r (tstr t)
	| ORef (r,v) -> Printf.sprintf "ref %d,&%d" r v
	| OUnref (v,r) -> Printf.sprintf "unref %d,*%d" v r
	| OSetref (r,v) -> Printf.sprintf "setref *%d,%d" r v
	| OToVirtual (r,v) -> Printf.sprintf "tovirtual %d,%d" r v
	| OUnVirtual (r,v) -> Printf.sprintf "unvirtual %d,%d" r v
	| ODynGet (r,o,f) -> Printf.sprintf "dynget %d,%d[@%d]" r o f
	| ODynSet (o,f,v) -> Printf.sprintf "dynset %d[@%d],%d" o f v
	| OMakeEnum (r,e,pl) -> Printf.sprintf "makeenum %d, %d(%s)" r e (String.concat "," (List.map string_of_int pl))
	| OEnumAlloc (r,e) -> Printf.sprintf "enumalloc %d, %d" r e
	| OEnumIndex (r,e) -> Printf.sprintf "enumindex %d, %d" r e
	| OEnumField (r,e,i,n) -> Printf.sprintf "enumfield %d, %d[%d:%d]" r e i n
	| OSetEnumField (e,i,r) -> Printf.sprintf "setenumfield %d[%d], %d" e i r
	| OSwitch (r,idx) -> Printf.sprintf "switch %d [%s]" r (String.concat "," (Array.to_list (Array.map string_of_int idx)))
	| ONullCheck r -> Printf.sprintf "nullcheck %d" r
	| OTrap (r,i) -> Printf.sprintf "trap %d, %d" r i
	| OEndTrap _ -> "endtrap"
	| ODump r -> Printf.sprintf "dump %d" r

let dump code =
	let lines = ref [] in
	let pr s =
		lines := s :: !lines
	in
	let all_protos = Hashtbl.create 0 in
	let tstr t =
		(match t with
		| HObj p -> Hashtbl.replace all_protos p.pname p
		| _ -> ());
		tstr t
	in
	let str idx =
		try
			code.strings.(idx)
		with _ ->
			"INVALID:" ^ string_of_int idx
	in
	let debug_infos (fid,line) =
		(try code.debugfiles.(fid) with _ -> "???") ^ ":" ^ string_of_int line
	in
	pr ("hl v" ^ string_of_int code.version);
	pr ("entry @" ^ string_of_int code.entrypoint);
	pr (string_of_int (Array.length code.strings) ^ " strings");
	Array.iteri (fun i s ->
		pr ("	@" ^ string_of_int i ^ " : " ^ s);
	) code.strings;
	pr (string_of_int (Array.length code.ints) ^ " ints");
	Array.iteri (fun i v ->
		pr ("	@" ^ string_of_int i ^ " : " ^ Int32.to_string v);
	) code.ints;
	pr (string_of_int (Array.length code.floats) ^ " floats");
	Array.iteri (fun i f ->
		pr ("	@" ^ string_of_int i ^ " : " ^ string_of_float f);
	) code.floats;
	pr (string_of_int (Array.length code.globals) ^ " globals");
	Array.iteri (fun i g ->
		pr ("	@" ^ string_of_int i ^ " : " ^ tstr g);
	) code.globals;
	pr (string_of_int (Array.length code.natives) ^ " natives");
	Array.iter (fun (lib,name,t,fidx) ->
		pr ("	@" ^ string_of_int fidx ^ " native " ^ str lib ^ "@" ^ str name ^ " " ^ tstr t);
	) code.natives;
	pr (string_of_int (Array.length code.functions) ^ " functions");
	Array.iter (fun f ->
		pr (Printf.sprintf "	@%d(%Xh) fun %s" f.findex f.findex (tstr f.ftype));
		pr (Printf.sprintf "	; %s" (debug_infos f.debug.(0)));
		Array.iteri (fun i r ->
			pr ("		r" ^ string_of_int i ^ " " ^ tstr r);
		) f.regs;
		Array.iteri (fun i o ->
			pr (Printf.sprintf "		.%-5d @%d %s" (snd f.debug.(i)) i (ostr o))
		) f.code;
	) code.functions;
	let protos = Hashtbl.fold (fun _ p acc -> p :: acc) all_protos [] in
	pr (string_of_int (List.length protos) ^ " objects protos");
	List.iter (fun p ->
		pr ("	" ^ p.pname);
		(match p.psuper with
		| None -> ()
		| Some p -> pr ("		extends " ^ p.pname));
		pr ("		" ^ string_of_int (Array.length p.pfields) ^ " fields");
		Array.iteri (fun i (_,id,t) ->
			pr ("		  @" ^ string_of_int i ^ " " ^ str id ^ " " ^ tstr t)
		) p.pfields;
		pr ("		" ^ string_of_int (Array.length p.pproto) ^ " methods");
		Array.iteri (fun i f ->
			pr ("		  @" ^ string_of_int i ^ " " ^ str f.fid ^ " fun@" ^ string_of_int f.fmethod ^ (match f.fvirtual with None -> "" | Some p -> "[" ^ string_of_int p ^ "]"))
		) p.pproto;
	) protos;
	String.concat "\n" (List.rev !lines)


(* --------------------------------------------------------------------------------------------------------------------- *)

let generate com =
	let get_class name =
		try
			match List.find (fun t -> (t_infos t).mt_path = (["hl";"types"],name)) com.types with
			| TClassDecl c -> c
			| _ -> assert false
		with
			Not_found ->
				assert false
	in
	let ctx = {
		com = com;
		m = method_context HVoid null_capture;
		cints = new_lookup();
		cstrings = new_lookup();
		cfloats = new_lookup();
		cglobals = new_lookup();
		cnatives = new_lookup();
		cfunctions = DynArray.create();
		overrides = Hashtbl.create 0;
		cached_types = PMap.empty;
		cfids = new_lookup();
		defined_funs = Hashtbl.create 0;
		array_impl = {
			aobj = get_class "ArrayObj";
			ai32 = get_class "ArrayI32";
			af64 = get_class "ArrayF64";
		};
		anons_cache = [];
		method_wrappers = PMap.empty;
		cdebug_files = new_lookup();
	} in
	ignore(alloc_string ctx "");
	let all_classes = Hashtbl.create 0 in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			let rec loop p f =
				match p with
				| Some (p,_) when PMap.mem f.cf_name p.cl_fields ->
					Hashtbl.replace ctx.overrides (f.cf_name,p.cl_path) true;
					loop p.cl_super f
				| _ -> ()
			in
			List.iter (fun f -> loop c.cl_super f) c.cl_overrides;
			Hashtbl.add all_classes c.cl_path c
 		| _ -> ()
	) com.types;
	List.iter (generate_type ctx) com.types;
	let ep = generate_static_init ctx in
	let code = {
		version = 1;
		entrypoint = ep;
		strings = DynArray.to_array ctx.cstrings.arr;
		ints = DynArray.to_array ctx.cints.arr;
		floats = DynArray.to_array ctx.cfloats.arr;
		globals = DynArray.to_array ctx.cglobals.arr;
		natives = DynArray.to_array ctx.cnatives.arr;
		functions = DynArray.to_array ctx.cfunctions;
		debugfiles = DynArray.to_array ctx.cdebug_files.arr;
	} in
	Array.sort (fun (lib1,_,_,_) (lib2,_,_,_) -> lib1 - lib2) code.natives;
	if Common.defined com Define.Dump then Std.output_file "dump/hlcode.txt" (dump code);
	PMap.iter (fun (s,p) fid ->
		if not (Hashtbl.mem ctx.defined_funs fid) then failwith ("Unresolved method " ^ s_type_path p ^ ":" ^ s)
	) ctx.cfids.map;
	check code;
	let ch = IO.output_string() in
	write_code ch code;
	let str = IO.close_out ch in
	let ch = open_out_bin com.file in
	output_string ch str;
	close_out ch;
	if Common.defined com Define.Interp then ignore(interp code)

