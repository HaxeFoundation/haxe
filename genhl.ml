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
	| HArray of ttype
	| HType

and class_proto = {
	pname : string;
	pid : int;
	mutable psuper : class_proto option;
	mutable pvirtuals : int array;
	mutable pproto : field_proto array;
	mutable pfields : (string * string index * ttype) array;
	mutable pindex : (string, int) PMap.t;
}

and field_proto = {
	fname : string;
	fid : int;
	fmethod : functable index;
	fvirtual : int option;
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
	| OSetGlobal of reg * global
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
	| OSetByte of reg * reg * reg
	| OSetArray of reg * reg * reg
	| OGetArray of reg * reg * reg
	| OUnsafeCast of reg * reg
	| OArraySize of reg * reg
	| OError of string index
	| OType of reg * ttype

type fundecl = {
	findex : functable index;
	ftype : ttype;
	regs : ttype array;
	code : opcode array;
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
}

(* compiler *)

type ('a,'b) lookup = {
	arr : 'b DynArray.t;
	mutable map : ('a, int) PMap.t;
}

type method_context = {
	mregs : (int, ttype) lookup;
	mops : opcode DynArray.t;
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
	array_impl : tclass;
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

let rec tstr ?(detailed=false) t =
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
	| HArray t ->
		"array(" ^ tstr t ^ ")"
	| HType ->
		"type"

let rec tsame t1 t2 =
	if t1 == t2 then true else
	match t1, t2 with
	| HFun (args1,ret1), HFun (args2,ret2) when List.length args1 = List.length args2 -> List.for_all2 tsame args1 args2 && tsame ret2 ret1
	| HObj p1, HObj p2 -> p1.pname = p2.pname
	| HDyn None, HDyn None -> true
	| HDyn (Some t1), HDyn (Some t2) -> tsame t1 t2
	| HArray t1, HArray t2 -> tsame t1 t2
	| _ -> false

let rec safe_cast t1 t2 =
	if t1 == t2 then true else
	match t1, t2 with
	| (HDyn _ | HObj _ | HFun _ | HArray _), HDyn None -> true
	| HDyn (Some t1), HDyn (Some t2) -> tsame t1 t2
	| HObj p1, HObj p2 ->
		let rec loop p =
			p.pname = p2.pname || (match p.psuper with None -> false | Some p -> loop p)
		in
		loop p1
	| _ -> tsame t1 t2

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

let lookup l v fb =
	try
		PMap.find v l.map
	with Not_found ->
		let id = DynArray.length l.arr in
		DynArray.add l.arr (Obj.magic 0);
		l.map <- PMap.add v id l.map;
		DynArray.set l.arr id (fb());
		id

let method_context() =
	{
		mregs = new_lookup();
		mops = DynArray.create();
	}

let field_name c f =
	s_type_path c.cl_path ^ ":" ^ f.cf_name

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
	| TAnon _ ->
		HDyn None
	| TDynamic _ ->
		HDyn None
	| TEnum (e,_) ->
		assert false
	| TInst ({ cl_path = [],"Array" },[t]) ->
		(match to_type ctx t with
		| HObj _ | HDyn _ | HFun _ | HArray _ -> class_type ctx ctx.array_impl
		| t -> failwith ("No support for Array<" ^ tstr t ^ "> yet"))
	| TInst (c,_) ->
		(match c.cl_kind with
		| KTypeParameter _ -> HDyn None
		| _ -> class_type ctx c)
	| TAbstract (a,pl) ->
		if Meta.has Meta.CoreType a.a_meta then
			(match a.a_path with
			| [], "Void" -> HVoid
			| [], "Int" -> HI32
			| [], "Float" -> HF64
			| [], "Bool" -> HBool
			| ["hl";"types"], "Bytes" -> HBytes
			| ["hl";"types"], "ArrayObject" -> HArray (to_type ctx (List.hd pl))
			| _ -> failwith ("Unknown core type " ^ s_type_path a.a_path))
		else
			to_type ctx (Abstract.get_underlying_type a pl)

and class_type ctx c =
	try
		PMap.find c.cl_path ctx.cached_types
	with Not_found ->
		let pname = s_type_path c.cl_path in
		let p = {
			pname = pname;
			pid = alloc_string ctx pname;
			psuper = None;
			pproto = [||];
			pfields = [||];
			pindex = PMap.empty;
			pvirtuals = [||];
		} in
		let t = HObj p in
		ctx.cached_types <- PMap.add c.cl_path t ctx.cached_types;
		let start_field, virtuals = (match c.cl_super with
			| None -> 0, [||]
			| Some (c,_) ->
				match class_type ctx c with
				| HObj psup ->
					p.psuper <- Some psup;
					p.pindex <- psup.pindex;
					Array.length p.pfields, p.pvirtuals
				| _ -> assert false
		) in
		let fa = DynArray.create() and pa = DynArray.create() and virtuals = DynArray.of_array virtuals in
		List.iter (fun f ->
			if is_extern_field f then () else
			match f.cf_kind with
			| Var _ | Method MethDynamic ->
				let t = to_type ctx f.cf_type in
				p.pindex <- PMap.add f.cf_name (DynArray.length fa + start_field) p.pindex;
				DynArray.add fa (f.cf_name, alloc_string ctx f.cf_name, t);
			| Method _ ->
				let g = alloc_fid ctx c f in
				let virt = if List.memq f c.cl_overrides then
					Some (try PMap.find f.cf_name p.pindex with Not_found -> assert false)
				else if is_overriden ctx c f then begin
					let vid = DynArray.length virtuals in
					DynArray.add virtuals g;
					p.pindex <- PMap.add f.cf_name vid p.pindex;
					Some vid
				end else
					None
				in
				DynArray.add pa { fname = f.cf_name; fid = alloc_string ctx f.cf_name; fmethod = g; fvirtual = virt; }
		) c.cl_ordered_fields;
		(try
			let cf = PMap.find "toString" c.cl_fields in
			if List.memq cf c.cl_overrides then raise Not_found;
			DynArray.add pa { fname = "__string"; fid = alloc_string ctx "__string"; fmethod = alloc_fun_path ctx c.cl_path "__string"; fvirtual = None; }
		with Not_found ->
			());
		p.pfields <- DynArray.to_array fa;
		p.pproto <- DynArray.to_array pa;
		p.pvirtuals <- DynArray.to_array virtuals;
		t

and alloc_fid ctx c f =
	match f.cf_kind with
	| Var _ | Method MethDynamic -> assert false
	| _ -> lookup ctx.cfids (f.cf_name, c.cl_path) (fun() -> ())

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
	if v.v_capture then assert false;
	lookup ctx.m.mregs v.v_id (fun() -> to_type ctx v.v_type)

let alloc_tmp ctx t =
	let rid = DynArray.length ctx.m.mregs.arr in
	DynArray.add ctx.m.mregs.arr t;
	rid

let op ctx o =
	DynArray.add ctx.m.mops o

let jump ctx f =
	let pos = DynArray.length ctx.m.mops in
	DynArray.add ctx.m.mops (OJAlways (-1)); (* loop *)
	(fun() -> DynArray.set ctx.m.mops pos (f (DynArray.length ctx.m.mops - pos - 1)))

let jump_back ctx =
	let pos = DynArray.length ctx.m.mops in
	DynArray.add ctx.m.mops (OLabel 0);
	(fun() -> DynArray.add ctx.m.mops (OJAlways (pos - DynArray.length ctx.m.mops - 1)))

let rtype ctx r =
	DynArray.get ctx.m.mregs.arr r

let resolve_field ctx p fname proto =
	try PMap.find fname p.pindex with Not_found -> assert false

let reg_int ctx v =
	let r = alloc_tmp ctx HI32 in
	op ctx (OInt (r,alloc_i32 ctx (Int32.of_int v)));
	r

let rec eval_to ctx e (t:ttype) =
	let r = eval_expr ctx e in
	cast_to ctx r t e.epos

and cast_to ctx (r:reg) (t:ttype) p =
	let rt = rtype ctx r in
	if safe_cast rt t then r else
	match rt, t with
	| _ , HDyn _ ->
		let tmp = alloc_tmp ctx (HDyn (Some rt)) in
		op ctx (OToDyn (tmp, r));
		tmp
	| (HI8 | HI16 | HI32), (HF32 | HF64) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToFloat (tmp, r));
		tmp
	| _ ->
		error ("Don't know how to cast " ^ tstr rt ^ " to " ^ tstr t) p

and get_access ctx e =
	match e.eexpr with
	| TField (ethis, a) ->
		(match a, follow ethis.etype with
		| FStatic (c,({ cf_kind = Var _ | Method MethDynamic } as f)), _ ->
			AGlobal (alloc_global ctx (field_name c f) f.cf_type)
		| FStatic (c,({ cf_kind = Method _ } as f)), _ ->
			AStaticFun (alloc_fid ctx c f)
		| FClosure (Some (cdef,_), ({ cf_kind = Method m } as f)), TInst (c,_)
		| FInstance (cdef,_,({ cf_kind = Method m } as f)), TInst (c,_) when m <> MethDynamic ->
			if not (is_overriden ctx c f) then
				AInstanceFun (ethis, alloc_fid ctx cdef f)
			else (match class_type ctx cdef with
			| HObj p -> AInstanceProto (ethis, resolve_field ctx p f.cf_name true)
			| _ -> assert false)
		| FInstance (cdef,_,f), _ | FClosure (Some (cdef,_), f), _ ->
			(match class_type ctx cdef with
			| HObj p -> AInstanceField (ethis, resolve_field ctx p f.cf_name false)
			| _ -> assert false)
		| _ ->
			ANone)
	| TLocal v ->
		ALocal (alloc_reg ctx v)
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
		let r1 = eval_expr ctx e1 in
		let r2 = eval_expr ctx e2 in
		let r1, r2 = (match rtype ctx r1, rtype ctx r2 with
			| (HI8 | HI16 | HI32), ((HF32 | HF64) as t) ->
				let tmp = alloc_tmp ctx t in
				op ctx (OToFloat (tmp,r1));
				tmp, r2
			| ((HF32 | HF64) as t), (HI8 | HI16 | HI32) ->
				let tmp = alloc_tmp ctx t in
				op ctx (OToFloat (tmp,r2));
				r1, tmp
			| t1, t2 ->
				if t1 == t2 then r1, r2 else error ("Don't know how to compare " ^ tstr t1 ^ " and " ^ tstr t2) e.epos
		) in
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
	List.map2 (fun e t -> eval_to ctx e t) el (match t with HFun (args,_) -> args | _ -> assert false)

and eval_null_check ctx e =
	let r = eval_expr ctx e in
	(match e.eexpr with
	| TConst TThis -> ()
	| _ ->
		let j = jump ctx (fun i -> OJNotNull (r,i)) in
		op ctx (OError (alloc_string ctx "Null access"));
		j());
	r

and eval_expr ctx e =
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
		let r = alloc_reg ctx v in
		(match e with
		| None -> ()
		| Some e ->
			let ri = eval_expr ctx e in
			op ctx (OMov (r,ri)));
		r
	| TLocal v ->
		alloc_reg ctx v
	| TReturn None ->
		let r = alloc_tmp ctx HVoid in
		op ctx (ORet r);
		r
	| TReturn (Some e) ->
		let r = eval_expr ctx e in
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
		(match v.v_name, el with
		| "$new", [{ eexpr = TTypeExpr (TClassDecl _) }] ->
			(match follow e.etype with
			| TInst (c,pl) ->
				let r = alloc_tmp ctx (class_type ctx c) in
				op ctx (ONew r);
				r
			| _ ->
				assert false)
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
		| "$bset", [b;pos;v] ->
			let b = eval_to ctx b HBytes in
			let pos = eval_to ctx pos HI32 in
			let r = eval_to ctx v HI32 in
			op ctx (OSetByte (b, pos, r));
			r
		| "$asize", [e] ->
			let r = alloc_tmp ctx HI32 in
			op ctx (OArraySize (r, eval_to ctx e (HArray (HDyn None))));
			r
		| "$aalloc", [esize] ->
			let et = (match follow e.etype with TAbstract ({ a_path = ["hl";"types"],"ArrayObject" },[t]) -> to_type ctx t | _ -> assert false) in
			(match et with
			| HObj _ | HArray _ | HFun _ | HDyn _ ->
				let a = alloc_tmp ctx (HArray (HDyn None)) in
				let rt = alloc_tmp ctx HType in
				op ctx (OType (rt,et));
				let size = eval_to ctx esize HI32 in
				op ctx (OCall2 (a,alloc_std ctx "aalloc" [HType;HI32] (HArray (HDyn None)),rt,size));
				a
			| _ ->
				assert false)
		| _ ->
			error ("Unknown native call " ^ v.v_name) e.epos)
	| TCall (ec,el) ->
		let ret = alloc_tmp ctx (to_type ctx e.etype) in
		let el = eval_args ctx el (to_type ctx ec.etype) in
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
		| _ ->
			let r = eval_expr ctx ec in
			op ctx (OCallClosure (ret, r, el)); (* if it's a value, it's a closure *)
		);
		ret
	| TField (ec,a) ->
		let r = alloc_tmp ctx (to_type ctx e.etype) in
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
		| AInstanceProto (ethis,fid) ->
			let robj = eval_null_check ctx ethis in
			op ctx (OMethod (r,robj,fid));
		| ANone | ALocal _ | AArray _ ->
			error "Invalid access" e.epos);
		r
	| TObjectDecl o ->
		(* TODO *)
		alloc_tmp ctx HVoid
	| TNew (c,pl,el) ->
		let r = alloc_tmp ctx (class_type ctx c) in
		op ctx (ONew r);
		(match c.cl_constructor with
		| None -> ()
		| Some { cf_expr = None } -> assert false
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
		(match bop with
		| OpLte ->
			let r = alloc_tmp ctx HBool in
			let a = eval_expr ctx e1 in
			let b = eval_expr ctx e2 in
			op ctx (gte r b a);
			r
		| OpGt ->
			let r = alloc_tmp ctx HBool in
			let a = eval_expr ctx e1 in
			let b = eval_expr ctx e2 in
			op ctx (lt r b a);
			r
		| OpGte ->
			let r = alloc_tmp ctx HBool in
			let a = eval_expr ctx e1 in
			let b = eval_expr ctx e2 in
			op ctx (gte r a b);
			r
		| OpLt ->
			let r = alloc_tmp ctx HBool in
			let a = eval_expr ctx e1 in
			let b = eval_expr ctx e2 in
			op ctx (lt r a b);
			r
		| OpAdd ->
			let t = to_type ctx e.etype in
			let r = alloc_tmp ctx t in
			(match t with
			| HI8 | HI16 | HI32 | HF32 | HF64 ->
				let a = eval_to ctx e1 t in
				let b = eval_to ctx e2 t in
				op ctx (OAdd (r,a,b));
				r
			| HObj { pname = "String" } ->
				op ctx (OCall2 (r,alloc_fun_path ctx ([],"String") "__add__",eval_to ctx e1 t,eval_to ctx e2 t));
				r
			| _ ->
				assert false)
		| OpSub | OpMult | OpDiv ->
			let t = to_type ctx e.etype in
			let r = alloc_tmp ctx t in
			(match t with
			| HI8 | HI16 | HI32 | HF32 | HF64 ->
				let a = eval_to ctx e1 t in
				let b = eval_to ctx e2 t in
				(match bop with
				| OpSub -> op ctx (OSub (r,a,b))
				| OpMult -> op ctx (OMul (r,a,b))
				| OpDiv -> op ctx (if unsigned e1.etype && unsigned e2.etype then OUDiv (r,a,b) else OSDiv (r,a,b))
				| _ -> assert false);
				r
			| _ ->
				assert false)
		| OpShl | OpShr | OpUShr | OpAnd | OpOr | OpXor ->
			let t = to_type ctx e.etype in
			let r = alloc_tmp ctx t in
			(match t with
			| HI8 | HI16 | HI32 ->
				let a = eval_to ctx e1 t in
				let b = eval_to ctx e2 t in
				(match bop with
				| OpShl -> op ctx (OShl (r,a,b))
				| OpShr -> op ctx (if unsigned e1.etype then OUShr (r,a,b) else OSShr (r,a,b))
				| OpUShr -> op ctx (OUShr (r,a,b))
				| OpAnd -> op ctx (OAnd (r,a,b))
				| OpOr -> op ctx (OOr (r,a,b))
				| OpXor -> op ctx (OXor (r,a,b))
				| _ -> ());
				r
			| _ ->
				assert false)
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
				op ctx (OCall2 (alloc_tmp ctx HVoid, alloc_fun_path ctx (["hl";"types"],"ArrayImpl") "__expand", a, idx));
				j();
				let arr = alloc_tmp ctx (HArray (HDyn None)) in
				op ctx (OField (arr,a,0));
				op ctx (OSetArray (arr,idx,v));
				v
			| ANone | AInstanceFun _ | AInstanceProto _ | AStaticFun _ ->
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
		| _ ->
			error ("TODO " ^ s_expr (s_type (print_context())) e) e.epos)
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
		| _ ->
			error ("TODO " ^ s_expr (s_type (print_context())) e) e.epos
		);
	| TFunction f ->
		let fid = alloc_function_name ctx ("function#" ^ string_of_int (DynArray.length ctx.cfunctions)) in
		make_fun ctx fid f None;
		let r = alloc_tmp ctx (to_type ctx e.etype) in
		op ctx (OGetFunction (r, fid));
		r
	| TThrow v ->
		op ctx (OThrow (eval_expr ctx v));
		alloc_tmp ctx HVoid (* not initialized *)
	| TWhile (cond,eloop,NormalWhile) ->
		let ret = jump_back ctx in
		let j = jump_expr ctx cond false in
		ignore(eval_expr ctx eloop);
		ret();
		j();
		alloc_tmp ctx HVoid
	| TWhile (cond,eloop,DoWhile) ->
		let start = jump ctx (fun p -> OJAlways p) in
		let ret = jump_back ctx in
		let j = jump_expr ctx cond false in
		start();
		ignore(eval_expr ctx eloop);
		ret();
		j();
		alloc_tmp ctx HVoid
	| TCast (v,None) ->
		eval_to ctx v (to_type ctx e.etype)
	| TArrayDecl el ->
		let r = alloc_tmp ctx (to_type ctx e.etype) in
		let et = (match follow e.etype with TInst (_,[t]) -> to_type ctx t | _ -> assert false) in
		(match et with
		| HObj _ | HFun _ | HDyn _ | HArray _ ->
			let a = alloc_tmp ctx (HArray (HDyn None)) in
			let rt = alloc_tmp ctx HType in
			op ctx (OType (rt,et));
			let size = reg_int ctx (List.length el) in
			op ctx (OCall2 (a,alloc_std ctx "aalloc" [HType;HI32] (HArray (HDyn None)),rt,size));
			List.iteri (fun i e ->
				let r = eval_to ctx e et in
				op ctx (OSetArray (a,reg_int ctx i,r));
			) el;
			op ctx (OCall1 (r, alloc_fun_path ctx (["hl";"types"],"ArrayImpl") "alloc", a))
		| _ -> assert false);
		r
	| TArray (a,i) ->
		let ra = eval_null_check ctx a in
		let ri = eval_to ctx i HI32 in
		let at = (match follow a.etype with TInst ({ cl_path = [],"Array" },[t]) -> to_type ctx t | _ -> assert false) in
		(match at with
		| HFun _ | HObj _ | HArray _ | HDyn _ ->
			let harr = alloc_tmp ctx (HArray (HDyn None)) in
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
			op ctx (OUnsafeCast (r,tmp));
			jend();
			r
		| _ ->
			assert false)
	| _ ->
		error ("TODO " ^ s_expr (s_type (print_context())) e) e.epos

and make_fun ctx fidx f cthis =
	let old = ctx.m in
	ctx.m <- method_context();
	let tthis = (match cthis with
	| None -> None
	| Some c ->
		let t = to_type ctx (TInst (c,[])) in
		ignore(alloc_tmp ctx t); (* index 0 *)
		Some t
	) in
	let args = List.map (fun (v,o) ->
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
		rtype ctx r
	) f.tf_args in
	ignore(eval_expr ctx f.tf_expr);
	let tret = to_type ctx f.tf_type in
	if tret = HVoid then op ctx (ORet (alloc_tmp ctx HVoid));
	let f = {
		findex = fidx;
		ftype = HFun ((match tthis with None -> args | Some t -> t :: args), tret);
		regs = DynArray.to_array ctx.m.mregs.arr;
		code = DynArray.to_array ctx.m.mops;
	} in
	ctx.m <- old;
	Hashtbl.add ctx.defined_funs fidx ();
	DynArray.add ctx.cfunctions f

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
			| [] ->
				make_fun ctx (alloc_fid ctx c f) (match f.cf_expr with Some { eexpr = TFunction f } -> f | _ -> assert false) None
			| _ :: l ->
				loop l
		in
		loop f.cf_meta


let generate_member ctx c f =
	match f.cf_kind with
	| Var _ -> ()
	| Method m ->
		make_fun ctx (alloc_fid ctx c f) (match f.cf_expr with Some { eexpr = TFunction f } -> f | _ -> assert false) (Some c);
		if f.cf_name = "toString" && not (List.memq f c.cl_overrides) then begin
			let p = f.cf_pos in
			(* function __string() return this.toString().bytes *)
			let ethis = mk (TConst TThis) (TInst (c,List.map snd c.cl_params)) p in
			let tstr = mk (TCall (mk (TField (ethis,FInstance(c,List.map snd c.cl_params,f))) f.cf_type p,[])) ctx.com.basic.tstring p in
			let cstr, cf_bytes = (try (match ctx.com.basic.tstring with TInst(c,_) -> c, PMap.find "bytes" c.cl_fields | _ -> assert false) with Not_found -> assert false) in
			let estr = mk (TReturn (Some (mk (TField (tstr,FInstance (cstr,[],cf_bytes))) cf_bytes.cf_type p))) ctx.com.basic.tvoid p in
			make_fun ctx (alloc_fun_path ctx c.cl_path "__string") { tf_expr = estr; tf_args = []; tf_type = cf_bytes.cf_type; } (Some c)
		end

let generate_type ctx t =
	match t with
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
	| TTypeDecl _ ->
		()
	| TAbstractDecl a when has_meta Meta.CoreType a.a_meta  ->
		()
	| TEnumDecl _ | TAbstractDecl _ ->
		let inf = t_infos t in
		error ("Unsupported generation for " ^ s_type_path inf.mt_path) inf.mt_pos

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
			let p = f.cf_pos in
			exprs := mk (TCall (mk (TField (mk (TTypeExpr t) t_dynamic p, FStatic (c,f))) f.cf_type p,[])) t_void p :: !exprs
		| _ ->
			assert false
	);
	let fid = alloc_function_name ctx "<entry>" in
	make_fun ctx fid { tf_expr = mk (TBlock (List.rev !exprs)) t_void null_pos; tf_args = []; tf_type = t_void } None;
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
		let reg r t =
			if not (safe_cast (rtype r) t) then error ("Register " ^ string_of_int r ^ " should be " ^ tstr t ^ " and not " ^ tstr (rtype r))
		in
		let numeric r =
			match rtype r with
			| HI8 | HI16 | HI32 | HF32 | HF64 -> ()
			| _ -> error ("Register " ^ string_of_int r ^ " should be numeric")
		in
		let int r =
			match rtype r with
			| HI8 | HI16 | HI32 -> ()
			| _ -> error ("Register " ^ string_of_int r ^ " should be integral")
		in
		let float r =
			match rtype r with
			| HF32 | HF64 -> ()
			| _ -> error ("Register " ^ string_of_int r ^ " should be float")
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
			| _ -> error ("Register " ^ string_of_int r ^ " should be object")
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
				| HObj _ | HDyn _ -> ()
				| t -> error (tstr t ^ " is not nullable"))
			| OAdd (r,a,b) | OSub (r,a,b) | OMul (r,a,b) | OSDiv (r,a,b) | OUDiv (r,a,b) ->
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
			| OGetGlobal (r,g) | OSetGlobal (r,g) ->
				reg r code.globals.(g)
			| OEq (r,a,b) | ONotEq (r, a, b) | OSLt (r, a, b) | OULt (r, a, b) | OSGte (r, a, b) | OUGte (r, a, b) ->
				reg r HBool;
				reg a (rtype b)
			| ORet r ->
				reg r tret
			| OJTrue (r,delta) | OJFalse (r,delta) ->
				reg r HBool;
				can_jump delta
			| OJNull (r,delta) | OJNotNull (r,delta) ->
				ignore(rtype r);
				can_jump delta
			| OJUGte (a,b,delta) | OJULt (a,b,delta) | OJSGte (a,b,delta) | OJSLt (a,b,delta) | OJEq (a,b,delta) | OJNeq (a,b,delta) ->
				reg a (rtype b);
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
				is_obj r
			| OField (r,o,fid) | OSetField (o,fid,r) ->
				reg r (tfield o fid false)
			| OGetThis (r,fid) | OSetThis(fid,r) ->
				reg r (tfield 0 fid false)
			| OGetFunction (r,f) ->
				reg r ftypes.(f)
			| OMethod (r,o,fid) ->
				(match tfield o fid true with
				| HFun (t :: tl, tret) ->
					reg o t;
					reg r (HFun (tl,tret));
				| _ -> assert false)
			| OClosure (r,f,arg) ->
				(match ftypes.(f) with
				| HFun (t :: tl, tret) ->
					reg arg t;
					reg r (HFun (tl,tret));
				| _ -> assert false);
			| OThrow r ->
				ignore(rtype r)
			| OSetByte (r,p,v) ->
				reg r HBytes;
				reg p HI32;
				reg v HI32;
			| OSetArray (a,i,v) ->
				(match rtype a with
				| HArray t -> reg v t
				| _ -> reg a (HArray (HDyn None)));
				reg i HI32;
			| OGetArray (v,a,i) ->
				reg a (HArray (rtype v));
				reg i HI32;
			| OUnsafeCast (a,b) ->
				ignore(rtype a);
				ignore(rtype b);
			| OArraySize (r,a) ->
				(match rtype a with
				| HArray _ -> ()
				| _ -> reg a (HArray (HDyn None)));
				reg r HI32
			| OError s ->
				ignore(code.strings.(s));
			| OType (r,_) ->
				reg r HType
		) f.code
		(* TODO : check that all path correctly initialize NULL values and reach a return *)
	in
	Array.iter (fun fd ->
		if fd.findex >= Array.length ftypes then failwith ("Invalid function index " ^ string_of_int fd.findex);
		if ftypes.(fd.findex) <> HVoid then failwith "Duplicate function bind";
		ftypes.(fd.findex) <- fd.ftype;
	) code.functions;
	Array.iter (fun (_,_,t,idx) ->
		if idx >= Array.length ftypes then failwith ("Invalid native function index " ^ string_of_int idx);
		if ftypes.(idx) <> HVoid then failwith "Duplicate function bind";
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

and vfunction =
	| FFun of fundecl
	| FNativeFun of string * (value list -> value)

and vobject = {
	vproto : vproto;
	vfields : value array;
}

and vproto = {
	vclass : class_proto;
	vmethods : vfunction array;
}

exception Return of value

let default t =
	match t with
	| HVoid | HFun _ | HDyn _ | HObj _ | HBytes | HArray _ | HType -> VNull
	| HI8 | HI16 | HI32 -> VInt Int32.zero
	| HF32 | HF64 -> VFloat 0.
	| HBool -> VBool false

exception Runtime_error of string
exception InterpThrow of value

let interp code =

	let globals = Array.map default code.globals in
	let functions = Array.create (Array.length code.functions + Array.length code.natives) (FNativeFun ("",(fun _ -> assert false))) in
	let cached_protos = Hashtbl.create 0 in
	let func f = Array.unsafe_get functions f in

	let rec get_proto p =
		try
			Hashtbl.find cached_protos p.pname
		with Not_found ->
			let meths, fields = (match p.psuper with None -> [||],[||] | Some p -> let p,f = get_proto p in p.vmethods, f) in
			let meths = Array.append meths (Array.map (fun f -> functions.(f)) p.pvirtuals) in
			let fields = Array.append fields (Array.map (fun (_,_,t) -> t) p.pfields) in
			let proto = ({ vclass = p; vmethods = meths },fields) in
			Hashtbl.replace cached_protos p.pname proto;
			proto
	in

	let new_obj t =
		match t with
		| HObj p ->
			let p, fields = get_proto p in
			{ vproto = p; vfields = Array.map default fields }
		| _ -> assert false
	in

	let error msg = raise (Runtime_error msg) in

	let rec vstr v =
		match v with
		| VNull -> "null"
		| VInt i -> Int32.to_string i ^ "i"
		| VFloat f -> string_of_float f ^ "f"
		| VBool b -> if b then "true" else "false"
		| VDyn (v,t) -> "dyn(" ^ vstr v ^ ")"
		| VObj o ->
			let p = "#" ^ o.vproto.vclass.pname in
			let fid = ref None in
			Array.iter (fun p -> if p.fname = "__string" then fid := Some p.fmethod) o.vproto.vclass.pproto;
			(match !fid with
			| None -> p
			| Some f -> p ^ ":" ^ vstr (fcall (func f) [v]))
		| VBytes b -> "bytes(" ^ (if String.length b > 0 && String.get b (String.length b - 1) = '\x00' then String.sub b 0 (String.length b - 1) else b) ^ ")"
		| VClosure (f,o) ->
			(match o with
			| None -> fstr f
			| Some v -> fstr f ^ "(" ^ vstr v ^ ")")
		| VArray (a,t) -> "array<" ^ tstr t ^ ">(" ^ String.concat "," (Array.to_list (Array.map vstr a)) ^ ")"
		| VUndef -> "undef"
		| VType t -> "type(" ^ tstr t ^ ")"

	and fstr = function
		| FFun f -> "function@" ^ string_of_int f.findex
		| FNativeFun (s,_) -> "native[" ^ s ^ "]"

	and fcall f args =
		match f with
		| FFun f -> call f args
		| FNativeFun (_,f) -> f args

	and call f args =
		let regs = Array.create (Array.length f.regs) VUndef in
		iteri (fun i v -> regs.(i) <- v) args;
		let pos = ref 0 in
		let rtype i = f.regs.(i) in
		let set r v = Array.unsafe_set regs r v in
		let get r = Array.unsafe_get regs r in
		let global g = Array.unsafe_get globals g in
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
			| OSetGlobal (r,g) -> Array.unsafe_set globals g (get r)
			| OEq (r,a,b) -> set r (VBool (get a = get b))
			| ONotEq (r,a,b) -> set r (VBool (get a <> get b))
			| OSGte (r,a,b) -> set r (VBool (get a >= get b))
			| OSLt (r,a,b) -> set r (VBool (get a < get b))
			| OUGte (r,a,b) -> set r (VBool (ucompare (get a) (get b) >= 0))
			| OULt (r,a,b) -> set r (VBool (ucompare (get a) (get b) < 0))
			| OJTrue (r,i) -> if get r = VBool true then pos := !pos + i
			| OJFalse (r,i) -> if get r = VBool false then pos := !pos + i
			| ORet r -> raise (Return regs.(r))
			| OJNull (r,i) -> if get r == VNull then pos := !pos + i
			| OJNotNull (r,i) -> if get r != VNull then pos := !pos + i
			| OJSLt (a,b,i) -> if get a < get b then pos := !pos + i
			| OJSGte (a,b,i) -> if get a >= get b then pos := !pos + i
			| OJULt (a,b,i) -> if ucompare (get a) (get b) < 0 then pos := !pos + i
			| OJUGte (a,b,i) -> if ucompare (get a) (get b) >= 0 then pos := !pos + i
			| OJEq (a,b,i) -> if get a = get b then pos := !pos + i
			| OJNeq (a,b,i) -> if get a <> get b then pos := !pos + i
			| OJAlways i -> pos := !pos + i
			| OToDyn (r,a) -> set r (VDyn (get a, f.regs.(a)))
			| OToFloat (r,a) -> set r (match get a with VInt v -> VFloat (Int32.to_float v) | _ -> assert false)
			| OToInt (r,a) -> set r (match get a with VFloat v -> VInt (Int32.of_float v) | _ -> assert false)
			| OLabel _ -> ()
			| ONew r -> set r (VObj (new_obj (rtype r)))
			| OField (r,o,fid) ->
				set r (match get o with VObj v -> v.vfields.(fid) | VNull -> error "Null access" | _ -> assert false)
			| OSetField (o,fid,r) ->
				(match get o with
				| VObj v -> v.vfields.(fid) <- get r
				| VNull -> error "Null access"
				| _ -> assert false)
			| OGetThis (r, fid) ->
				set r (match get 0 with VObj v -> v.vfields.(fid) | _ -> assert false)
			| OSetThis (fid, r) ->
				(match get 0 with
				| VObj v -> v.vfields.(fid) <- get r
				| _ -> assert false)
			| OCallMethod (r,m,rl) ->
				(match get (List.hd rl) with
				| VObj v -> set r (fcall v.vproto.vmethods.(m) (List.map get rl))
				| VNull -> error "Null access"
				| _ -> assert false)
			| OCallThis (r,m,rl) ->
				(match get 0 with
				| VObj v as o -> set r (fcall v.vproto.vmethods.(m) (o :: List.map get rl))
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
				(match get o with
				| VObj v as obj -> set r (VClosure (v.vproto.vmethods.(m), Some obj))
				| VNull -> error "Null access"
				| _ -> assert false)
			| OThrow r ->
				raise (InterpThrow (get r))
			| OSetByte (r,p,v) ->
				(match get r, get p, get v with
				| VBytes b, VInt p, VInt v -> String.set b (Int32.to_int p) (char_of_int ((Int32.to_int v) land 0xFF))
				| _ -> assert false)
			| OSetArray (a,i,v) ->
				(match get a, get i with
				| VArray (a,_), VInt i -> a.(Int32.to_int i) <- get v
				| _ -> assert false);
			| OGetArray (r,a,i) ->
				(match get a, get i with
				| VArray (a,_), VInt i -> set r a.(Int32.to_int i)
				| _ -> assert false);
			| OUnsafeCast (r,v) ->
				set r (get v)
			| OArraySize (r,a) ->
				(match get a with
				| VArray (a,_) -> set r (VInt (Int32.of_int (Array.length a)));
				| _ -> assert false)
			| OError s ->
				raise (InterpThrow (VDyn (VBytes (code.strings.(s) ^ "\x00"),HBytes)))
			| OType (r,t) ->
				set r (VType t)
			);
			loop()
		in
		try
			loop()
		with
			Return v -> v
	in
	let load_native lib name =
		FNativeFun (lib ^ "@" ^ name,match lib, name with
		| "std", "log" ->
			(fun args -> print_endline (vstr (List.hd args)); VNull);
		| "std", "balloc" ->
			(function
			| [VInt i] -> VBytes (String.create (Int32.to_int i))
			| _ -> assert false)
		| "std", "aalloc" ->
			(function
			| [VType t;VInt i] -> VArray (Array.create (Int32.to_int i) VNull,t)
			| _ -> assert false)
		| "std", "ablit" ->
			(function
			| [VArray (dst,_); VInt dp; VArray (src,_); VInt sp; VInt len] ->
				Array.blit src (Int32.to_int sp) dst (Int32.to_int dp) (Int32.to_int len);
				VNull
			| _ -> assert false)
		| "std", "bblit" ->
			(function
			| [VBytes dst; VInt dp; VBytes src; VInt sp; VInt len] ->
				String.blit src (Int32.to_int sp) dst (Int32.to_int dp) (Int32.to_int len);
				VNull
			| _ -> assert false)
		| _ -> (fun args -> error ("Unresolved native " ^ name))
		)
	in
	Array.iter (fun (lib,name,_,idx) -> functions.(idx) <- load_native code.strings.(lib) code.strings.(name)) code.natives;
	Array.iter (fun fd -> functions.(fd.findex) <- FFun fd) code.functions;
	match functions.(code.entrypoint) with
	| FFun f when f.ftype = HFun([],HVoid) -> (try call f [] with InterpThrow v -> error ("Uncaught exception " ^ vstr v))
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
		| OLabel _ ->
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
		| OCallN (r,f,rl) | OCallClosure (r,f,rl) | OCallMethod (r,f,rl) | OCallThis (r,f,rl) ->
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
			| HDyn (Some t) | HArray t ->
				get_type t
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
		| HArray t ->
			byte 11;
			write_type t
		| HType ->
			byte 12
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
	| OSetByte (r,p,v) -> Printf.sprintf "setbyte %d,%d,%d" r p v
	| OSetArray (a,i,v) -> Printf.sprintf "setarray %d[%d],%d" a i v
	| OGetArray (r,a,i) -> Printf.sprintf "getarray %d,%d[%d]" r a i
	| OUnsafeCast (r,v) -> Printf.sprintf "unsafecast %d,%d" r v
	| OArraySize (r,a) -> Printf.sprintf "arraysize %d,%d" r a
	| OError s -> Printf.sprintf "error @%d" s
	| OType (r,t) -> Printf.sprintf "type %d,%s" r (tstr t)

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
		Array.iteri (fun i r ->
			pr ("		r" ^ string_of_int i ^ " " ^ tstr r);
		) f.regs;
		Array.iteri (fun i o ->
			pr ("		@"  ^ string_of_int i ^ " " ^ ostr o);
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
		m = method_context();
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
		array_impl = get_class "ArrayImpl";
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
	PMap.iter (fun (s,p) fid ->
		if not (Hashtbl.mem ctx.defined_funs fid) then failwith ("Unresolved method " ^ s_type_path p ^ ":" ^ s)
	) ctx.cfids.map;
	let code = {
		version = 1;
		entrypoint = ep;
		strings = DynArray.to_array ctx.cstrings.arr;
		ints = DynArray.to_array ctx.cints.arr;
		floats = DynArray.to_array ctx.cfloats.arr;
		globals = DynArray.to_array ctx.cglobals.arr;
		natives = DynArray.to_array ctx.cnatives.arr;
		functions = DynArray.to_array ctx.cfunctions;
	} in
	Array.sort (fun (lib1,_,_,_) (lib2,_,_,_) -> lib1 - lib2) code.natives;
	if Common.defined com Define.Dump then print_endline (dump code);
	check code;
	let ch = IO.output_string() in
	write_code ch code;
	let str = IO.close_out ch in
	let ch = open_out_bin com.file in
	output_string ch str;
	close_out ch;
	if Common.defined com Define.Interp then ignore(interp code)

