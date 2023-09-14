(*
 * Copyright (C)2005-2019 Haxe Foundation
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
open Extlib_leftovers
open Unix
open Hlcode

type value =
	| VNull
	| VInt of int32
	| VInt64 of int64
	| VFloat of float
	| VBool of bool
	| VDyn of value * ttype
	| VObj of vobject
	| VClosure of vfunction * value option
	| VBytes of string
	| VArray of value array * ttype
	| VUndef
	| VType of ttype
	| VRef of ref_value * ttype
	| VVirtual of vvirtual
	| VDynObj of vdynobj
	| VEnum of enum_proto * int * value array
	| VAbstract of vabstract
	| VVarArgs of vfunction * value option
	| VStruct of vobject

and ref_value =
	| RStack of int
	| RValue of value ref
	| RArray of value array * int

and vabstract =
	| AHashBytes of (string, value) Hashtbl.t
	| AHashInt of (int32, value) Hashtbl.t
	| AHashObject of (value * value) list ref
	| AReg of regexp
	| ARandom
	| APos of Globals.pos
	| ATDecl of Type.module_type
	| AUnsafe of Obj.t
	| ALazyType of ((unit -> Type.t) ref) * (unit -> value)

and vfunction =
	| FFun of fundecl
	| FNativeFun of string * (value list -> value) * ttype

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
	mutable vindexes : vfield array;
	mutable vtable : value array;
	mutable vvalue : value;
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

and regexp = {
	r : Str.regexp;
	mutable r_string : string;
	mutable r_groups : (int * int) option array;
}

exception Return of value
exception Runtime_error of string
exception InterpThrow of value
exception Sys_exit of int

type context = {
	mutable t_globals : value array;
	mutable t_functions : vfunction array;
	mutable call_stack : (fundecl * int ref) list;
	mutable error_stack : (fundecl * int ref) list;
	mutable stack : value array;
	mutable stack_pos : int;
	mutable fcall : vfunction -> value list -> value;
	mutable code : code;
	mutable on_error : value -> (fundecl * int ref) list -> unit;
	mutable resolve_macro_api : string -> (value list -> value) option;
	checked : bool;
	cached_protos : (int, vproto * ttype array * (int * (value -> value)) list) Hashtbl.t;
	cached_strings : (int, string) Hashtbl.t;
	cached_hashes : (int32, string) Hashtbl.t;
}

let default t =
	match t with
	| HUI8 | HUI16 | HI32 -> VInt Int32.zero
	| HI64 -> VInt64 Int64.zero
	| HF32 | HF64 -> VFloat 0.
	| HBool -> VBool false
	| _ -> if is_nullable t then VNull else VUndef

let get_type = function
	| VDyn (_,t) -> Some t
	| VObj o -> Some (HObj o.oproto.pclass)
	| VDynObj _ -> Some HDynObj
	| VVirtual v -> Some (HVirtual v.vtype)
	| VArray _ -> Some HArray
	| VClosure (f,None) -> Some (match f with FFun f -> f.ftype | FNativeFun (_,_,t) -> t)
	| VClosure (f,Some _) -> Some (match f with FFun { ftype = HFun(_::args,ret) } | FNativeFun (_,_,HFun(_::args,ret)) -> HFun (args,ret) | _ -> Globals.die "" __LOC__)
	| VVarArgs _ -> Some (HFun ([],HDyn))
	| VEnum (e,_,_) -> Some (HEnum e)
	| _ -> None

let v_dynamic = function
	| VNull	| VDyn _ | VObj _ | VClosure _ | VArray _ | VVirtual _ | VDynObj _ | VVarArgs _ | VEnum _ -> true
	| _ -> false

let rec is_compatible v t =
	match v, t with
	| VInt _, (HUI8 | HUI16 | HI32) -> true
	| VInt64 _, HI64 -> true
	| VFloat _, (HF32 | HF64) -> true
	| VBool _, HBool -> true
	| _, HVoid -> true
	| VNull, t -> is_nullable t
	| VObj o, HObj _ -> safe_cast (HObj o.oproto.pclass) t
	| VClosure _, HFun _ -> safe_cast (match get_type v with None -> Globals.die "" __LOC__ | Some t -> t) t
	| VBytes _, HBytes -> true
	| VDyn (_,t1), HNull t2 -> tsame t1 t2
	| v, HNull t -> is_compatible v t
	| v, HDyn -> v_dynamic v
	| VType _, HType -> true
	| VArray _, HArray -> true
	| VDynObj _, HDynObj -> true
	| VVirtual v, HVirtual _ -> safe_cast (HVirtual v.vtype) t
	| VRef (_,t1), HRef t2 -> tsame t1 t2
	| VAbstract _, HAbstract _ -> true
	| VEnum _, HEnum _ -> true
	| VStruct v, HStruct _ -> safe_cast (HStruct v.oproto.pclass) t
	| _ -> false

type cast =
	| CNo
	| CDyn of ttype
	| CUnDyn of ttype
	| CCast of ttype * ttype

let error msg = raise (Runtime_error msg)

let get_function ctx f =
	ctx.t_functions.(f)

let rec get_proto ctx p =
	try
		Hashtbl.find ctx.cached_protos p.pid
	with Not_found ->
		let fields, bindings = (match p.psuper with None -> [||],[] | Some p -> let _, fields, bindings = get_proto ctx p in fields, bindings) in
		let meths = Array.map (get_function ctx) p.pvirtuals in
		let fields = Array.append fields (Array.map (fun (_,_,t) -> t) p.pfields) in
		let bindings = List.fold_left (fun acc (fid,fidx) ->
			let f = get_function ctx fidx in
			let ft = (match f with FFun f -> f.ftype | FNativeFun _ -> Globals.die "" __LOC__) in
			let need_closure = (match ft, fields.(fid) with HFun (args,_), HFun(args2,_) -> List.length args > List.length args2 | HFun _, HDyn -> false | _ -> Globals.die "" __LOC__) in
			let acc = List.filter (fun (fid2,_) -> fid2 <> fid) acc in
			(fid, (fun v -> VClosure (f,if need_closure then Some v else None))) :: acc
		) bindings p.pbindings in
		let proto = ({ pclass = p; pmethods = meths },fields,bindings) in
		Hashtbl.replace ctx.cached_protos p.pid proto;
		proto

let alloc_obj ctx t =
	match t with
	| HDynObj ->
		VDynObj { dfields = Hashtbl.create 0; dvalues = [||]; dtypes = [||]; dvirtuals = []; }
	| HObj p ->
		let p, fields, bindings = get_proto ctx p in
		let ftable = Array.map default fields in
		let obj = VObj { oproto = p; ofields = ftable } in
		List.iter (fun (fid,mk) -> ftable.(fid) <- mk obj) bindings;
		obj
	| HStruct p ->
		let p, fields, bindings = get_proto ctx p in
		let ftable = Array.map default fields in
		VStruct { oproto = p; ofields = ftable }
	| HVirtual v ->
		let o = {
			dfields = Hashtbl.create 0;
			dvalues = Array.map (fun (_,_,t) -> default t) v.vfields;
			dtypes = Array.map (fun (_,_,t) -> t) v.vfields;
			dvirtuals = [];
		} in
		Array.iteri (fun i (n,_,_) -> Hashtbl.add o.dfields n i) v.vfields;
		let v = { vtype = v; vvalue = VDynObj o; vtable = o.dvalues; vindexes = Array.mapi (fun i _ -> VFIndex i) v.vfields } in
		o.dvirtuals <- [v];
		VVirtual v
	| _ ->
		Globals.die "" __LOC__

let float_to_string f =
	let s = string_of_float f in
	let len = String.length s in
	if String.unsafe_get s (len - 1) = '.' then String.sub s 0 (len - 1) else s

let rec get_method p name =
	let m = ref None in
	Array.iter (fun p -> if p.fname = name then m := Some p.fmethod) p.pproto;
	match !m , p.psuper with
	| Some i, _ -> Some i
	| None, Some s -> get_method s name
	| None, None -> None

let get_to_string ctx p =
	match get_method p "__string" with
	| Some f ->
		(match get_function ctx f with
		| (FFun { ftype = HFun([_],HBytes) } as f) -> Some f
		| _ -> None)
	| None ->
		None

let set_i32 b p v =
	try
		Bytes.set (Bytes.unsafe_of_string b) p (char_of_int ((Int32.to_int v) land 0xFF));
		Bytes.set (Bytes.unsafe_of_string b) (p+1) (char_of_int ((Int32.to_int (Int32.shift_right_logical v 8)) land 0xFF));
		Bytes.set (Bytes.unsafe_of_string b) (p+2) (char_of_int ((Int32.to_int (Int32.shift_right_logical v 16)) land 0xFF));
		Bytes.set (Bytes.unsafe_of_string b) (p+3) (char_of_int (Int32.to_int (Int32.shift_right_logical v 24)));
	with _ ->
		error "Set outside of bytes bounds"

let set_i64 b p v =
	set_i32 b p (Int64.to_int32 v);
	set_i32 b (p + 4) (Int64.to_int32 (Int64.shift_right_logical v 32))

let get_i32 b p =
	let i = int_of_char (String.get b p) in
	let j = int_of_char (String.get b (p + 1)) in
	let k = int_of_char (String.get b (p + 2)) in
	let l = int_of_char (String.get b (p + 3)) in
	Int32.logor (Int32.of_int (i lor (j lsl 8) lor (k lsl 16))) (Int32.shift_left (Int32.of_int l) 24)

let get_i64 b p =
	let low = get_i32 b p in
	let high = get_i32 b (p + 4) in
	Int64.logor (Int64.logand (Int64.of_int32 low) 0xFFFFFFFFL) (Int64.shift_left (Int64.of_int32 high) 32)

let make_dyn v t =
	if v = VNull || is_dynamic t then
		v
	else
		VDyn (v,t)

let get_ref ctx = function
	| RStack i -> ctx.stack.(i)
	| RValue r -> !r
	| RArray (a,i) -> a.(i)

let set_ref ctx r v =
	match r with
	| RStack i -> ctx.stack.(i) <- v
	| RValue r -> r := v
	| RArray (a,i) -> a.(i) <- v

let fstr = function
	| FFun f -> "function@" ^ string_of_int f.findex
	| FNativeFun (s,_,_) -> "native[" ^ s ^ "]"

let caml_to_hl str = Common.utf8_to_utf16 str true

let hash ctx str =
	let h = hl_hash str in
	if not (Hashtbl.mem ctx.cached_hashes h) then Hashtbl.add ctx.cached_hashes h (String.sub str 0 (try String.index str '\000' with _ -> String.length str));
	h

let utf16_iter f s =
	let get v = int_of_char s.[v] in
	let rec loop p =
		if p = String.length s then () else
		let c = (get p) lor ((get (p+1)) lsl 8) in
		if c >= 0xD800 && c <= 0xDFFF then begin
			let c = c - 0xD800 in
			let c2 = ((get (p+2)) lor ((get(p+3)) lsl 8)) - 0xDC00 in
			f ((c2 lor (c lsl 10)) + 0x10000);
			loop (p + 4);
		end else begin
			f c;
			loop (p + 2);
		end;
	in
	loop 0

let utf16_char buf c =
	Common.utf16_add buf (int_of_char c)

let hl_to_caml str =
	let utf16_eof s =
		let get v = int_of_char s.[v] in
		let rec loop p =
			let c = (get p) lor ((get (p+1)) lsl 8) in
			if c = 0 then String.sub s 0 p else loop (p + 2);
		in
		loop 0
	in
	let b = UTF8.Buf.create (String.length str / 2) in
	utf16_iter (fun c -> UTF8.Buf.add_char b (UCharExt.chr c)) (utf16_eof str);
	UTF8.Buf.contents b

let null_access() =
	error "Null value bypass null pointer check"

let throw ctx v =
	ctx.error_stack <- [];
	raise (InterpThrow v)

let throw_msg ctx msg =
	throw ctx (VDyn (VBytes (caml_to_hl msg),HBytes))

let rec vstr_d ctx v =
	let vstr_d = vstr_d ctx in
	match v with
	| VNull -> "null"
	| VInt i -> Int32.to_string i ^ "i"
	| VInt64 i -> Int64.to_string i ^ "l"
	| VFloat f -> string_of_float f ^ "f"
	| VBool b -> if b then "true" else "false"
	| VDyn (v,t) -> "dyn(" ^ vstr_d v ^ ":" ^ tstr t ^ ")"
	| VObj o ->
		let p = o.oproto.pclass.pname in
		(match get_to_string ctx o.oproto.pclass with
		| Some f -> p ^ ":" ^ vstr_d (ctx.fcall f [v])
		| None -> p)
	| VBytes b -> "bytes(" ^ String.escaped b ^ ")"
	| VClosure (f,o) ->
		(match o with
		| None -> fstr f
		| Some v -> fstr f ^ "[" ^ vstr_d v ^ "]")
	| VArray (a,t) -> "array<" ^ tstr t ^ ">(" ^ String.concat "," (Array.to_list (Array.map vstr_d a)) ^ ")"
	| VUndef -> "undef"
	| VType t -> "type(" ^ tstr t ^ ")"
	| VRef (r,_) -> "ref(" ^ vstr_d (get_ref ctx r) ^ ")"
	| VVirtual v -> "virtual(" ^ vstr_d v.vvalue ^ ")"
	| VDynObj d -> "dynobj(" ^ String.concat "," (Hashtbl.fold (fun f i acc -> (f^":"^vstr_d d.dvalues.(i)) :: acc) d.dfields []) ^ ")"
	| VEnum (e,i,vals) -> let n, _, _ = e.efields.(i) in if Array.length vals = 0 then n else n ^ "(" ^ String.concat "," (Array.to_list (Array.map vstr_d vals)) ^ ")"
	| VAbstract _ -> "abstract"
	| VVarArgs _ -> "varargs"
	| VStruct v -> "@" ^ v.oproto.pclass.pname

let rec to_virtual ctx v vp =
	match v with
	| VNull ->
		VNull
	| VObj o ->
		let indexes = Array.mapi (fun i (n,_,t) ->
			try
				let idx, ft = get_index n o.oproto.pclass in
				if idx < 0 || not (tsame t ft) then raise Not_found;
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
	| VDynObj d ->
		(try
			VVirtual (List.find (fun v -> v.vtype == vp) d.dvirtuals)
		with Not_found ->
			let indexes = Array.mapi (fun i (n,_,t) ->
				try
					let idx = Hashtbl.find d.dfields n in
					if not (tsame t d.dtypes.(idx)) then raise Not_found;
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
	| VVirtual vd ->
		if vd.vtype == vp then
			v
		else if vd.vvalue = VNull then
			Globals.die "" __LOC__
		else
			to_virtual ctx vd.vvalue vp
	| _ ->
		throw_msg ctx ("Invalid ToVirtual " ^ vstr_d ctx v ^ " : " ^ tstr (HVirtual vp))

let rec dyn_cast ctx v t rt =
	let invalid() =
		throw_msg ctx ("Can't cast " ^ vstr_d ctx v ^ ":"  ^ tstr t ^ " to " ^ tstr rt)
	in
	let default() =
		let v = default rt in
		if v = VUndef then invalid();
		v
	in
	if safe_cast t rt then
		v
	else if v = VNull then
		default()
	else match t, rt with
	| (HUI8|HUI16|HI32), (HF32|HF64) ->
		(match v with VInt i -> VFloat (Int32.to_float i) | _ -> Globals.die "" __LOC__)
	| (HF32|HF64), (HUI8|HUI16|HI32) ->
		(match v with VFloat f -> VInt (Int32.of_float f) | _ -> Globals.die "" __LOC__)
	| (HUI8|HUI16|HI32|HF32|HF64), HNull ((HUI8|HUI16|HI32|HF32|HF64) as rt) ->
		let v = dyn_cast ctx v t rt in
		VDyn (v,rt)
	| HBool, HNull HBool ->
		VDyn (v,HBool)
	| _, HDyn ->
		make_dyn v t
	| _, HRef t2 when t = t2 ->
		VRef (RValue (ref v),t)
	| HFun (args1,t1), HFun (args2,t2) when List.length args1 = List.length args2 ->
		(match v with
		| VClosure (fn,farg) ->
			let get_conv t1 t2 =
				if safe_cast t1 t2 || (t2 = HDyn && is_dynamic t1) then CNo
				else if t2 = HDyn then CDyn t1
				else if t1 = HDyn then CUnDyn t2
				else CCast (t1,t2)
			in
			let conv = List.map2 get_conv args2 args1 in
			let rconv = get_conv t1 t2 in
			let convert v c =
				match c with
				| CNo -> v
				| CDyn t -> make_dyn v t
				| CUnDyn t -> dyn_cast ctx v HDyn t
				| CCast (t1,t2) -> dyn_cast ctx v t1 t2
			in
			VClosure (FNativeFun ("~convert",(fun args ->
				let args = List.map2 convert args conv in
				let ret = ctx.fcall fn (match farg with None -> args | Some a -> a :: args) in
				convert ret rconv
			),rt),None)
		| _ ->
			Globals.die "" __LOC__)
	| HDyn, HFun (targs,tret) when (match v with VVarArgs _ -> true | _ -> false) ->
		VClosure (FNativeFun ("~varargs",(fun args ->
			dyn_call ctx v (List.map2 (fun v t -> (v,t)) args targs) tret
		),rt),None)
	| HDyn, _ ->
		(match get_type v with
		| None -> Globals.die "" __LOC__
		| Some t -> dyn_cast ctx (match v with VDyn (v,_) -> v | _ -> v) t rt)
	| HNull t, _ ->
		(match v with
		| VDyn (v,t) -> dyn_cast ctx v t rt
		| _ -> Globals.die "" __LOC__)
	| HObj _, HObj b when safe_cast rt t && (match get_type v with Some t -> safe_cast t rt | None -> Globals.die "" __LOC__) ->
		(* downcast *)
		v
	| (HObj _ | HDynObj | HVirtual _), HVirtual vp ->
		to_virtual ctx v vp
	| HVirtual _, _ ->
		(match v with
		| VVirtual v -> dyn_cast ctx v.vvalue (match get_type v.vvalue with None -> Globals.die "" __LOC__ | Some t -> t) rt
		| _ -> Globals.die "" __LOC__)
	| HObj p, _ ->
		(match get_method p "__cast" with
		| None -> invalid()
		| Some f ->
			if v = VNull then VNull else
			let ret = ctx.fcall (get_function ctx f) [v;VType rt] in
			if ret <> VNull && (match get_type ret with None -> Globals.die "" __LOC__ | Some vt -> safe_cast vt rt) then ret else invalid())
	| _ ->
		invalid()

and dyn_call ctx v args tret =
	match v with
	| VClosure (f,a) ->
		let ft = (match f with FFun f -> f.ftype | FNativeFun (_,_,t) -> t) in
		let fargs, fret = (match ft with HFun (a,t) -> a, t | _ -> Globals.die "" __LOC__) in
		let full_args = args and full_fargs = (match a with None -> fargs | Some _ -> List.tl fargs) in
		let rec loop args fargs =
			match args, fargs with
			| [], [] -> []
			| _, [] -> throw_msg ctx (Printf.sprintf "Too many arguments (%s) != (%s)" (String.concat "," (List.map (fun (v,_) -> vstr_d ctx v) full_args)) (String.concat "," (List.map tstr full_fargs)))
			| (v,t) :: args, ft :: fargs -> dyn_cast ctx v t ft :: loop args fargs
			| [], _ :: fargs -> default ft :: loop args fargs
		in
		let vargs = loop args full_fargs in
		let v = ctx.fcall f (match a with None -> vargs | Some a -> a :: vargs) in
		dyn_cast ctx v fret tret
	| VNull ->
		null_access()
	| VVarArgs (f,a) ->
		let arr = VArray (Array.of_list (List.map (fun (v,t) -> make_dyn v t) args),HDyn) in
		dyn_call ctx (VClosure (f,a)) [arr,HArray] tret
	| _ ->
		throw_msg ctx (vstr_d ctx v ^ " cannot be called")

let invalid_comparison = 255

let rec dyn_compare ctx a at b bt =
	if a == b && (match at with HF32 | HF64 -> false | _ -> true) then 0 else
	let fcompare (a:float) (b:float) = if a = b then 0 else if a > b then 1 else if a < b then -1 else invalid_comparison in
	match a, b with
	| VInt a, VInt b -> Int32.compare a b
	| VInt a, VFloat b -> fcompare (Int32.to_float a) b
	| VFloat a, VInt b -> fcompare a (Int32.to_float b)
	| VFloat a, VFloat b -> fcompare a b
	| VBool a, VBool b -> compare a b
	| VNull, VNull -> 0
	| VType t1, VType t2 -> if tsame t1 t2 then 0 else 1
	| VNull, _ -> 1
	| _, VNull -> -1
	| VObj oa, VObj ob ->
		if oa == ob then 0 else
		(match get_method oa.oproto.pclass "__compare" with
		| None -> invalid_comparison
		| Some f -> (match ctx.fcall (get_function ctx f) [a;b] with VInt i -> Int32.to_int i | _ -> Globals.die "" __LOC__));
	| VDyn (v,t), _ ->
		dyn_compare ctx v t b bt
	| _, VDyn (v,t) ->
		dyn_compare ctx a at v t
	| VVirtual v, _ ->
		dyn_compare ctx v.vvalue HDyn b bt
	| _, VVirtual v ->
		dyn_compare ctx a at v.vvalue HDyn
	| _ ->
		invalid_comparison

let rec dyn_get_field ctx obj field rt =
	let get_with v t = dyn_cast ctx v t rt in
	match obj with
	| VDynObj d ->
		(try
			let idx = Hashtbl.find d.dfields field in
			get_with d.dvalues.(idx) d.dtypes.(idx)
		with Not_found ->
			default rt)
	| VObj o | VDyn (VStruct o, HStruct _) ->
		let default rt =
			match get_method o.oproto.pclass "__get_field" with
			| None -> default rt
			| Some f ->
				get_with (ctx.fcall (get_function ctx f) [obj; VInt (hash ctx field)]) HDyn
		in
		let rec loop p =
			try
				let fid = PMap.find field p.pfunctions in
				(match get_function ctx fid with
				| FFun fd as f -> get_with (VClosure (f,Some obj)) (match fd.ftype with HFun (_::args,t) -> HFun(args,t) | _ -> Globals.die "" __LOC__)
				| FNativeFun _ -> Globals.die "" __LOC__)
			with Not_found ->
				match p.psuper with
				| None -> default rt
				| Some p -> loop p
		in
		(try
			let idx, t = get_index field o.oproto.pclass in
			if idx < 0 then raise Not_found;
			get_with o.ofields.(idx) t
		with Not_found ->
			loop o.oproto.pclass)
	| VVirtual vp ->
		(match vp.vvalue with
		| VNull ->
			(try
				let idx = PMap.find field vp.vtype.vindex in
				match vp.vindexes.(idx) with
				| VFNone -> VNull
				| VFIndex i -> vp.vtable.(i)
			with Not_found ->
				VNull)
		| v -> dyn_get_field ctx v field rt)
	| VNull ->
		null_access()
	| _ ->
		throw_msg ctx "Invalid object access"

let rebuild_virtuals ctx d =
	let old = d.dvirtuals in
	d.dvirtuals <- [];
	List.iter (fun v ->
		let v2 = (match to_virtual ctx (VDynObj d) v.vtype with VVirtual v -> v | _ -> Globals.die "" __LOC__) in
		v.vindexes <- v2.vindexes;
		v.vtable <- d.dvalues;
	) old;
	d.dvirtuals <- old

let rec dyn_set_field ctx obj field v vt =
	let v, vt = (match vt with
		| HDyn ->
			(match get_type v with
			| None -> if v = VNull then VNull, HDyn else Globals.die "" __LOC__
			| Some t -> (match v with VDyn (v,_) -> v | _ -> v), t)
		| t -> v, t
	) in
	match obj with
	| VDynObj d ->
		(try
			let idx = Hashtbl.find d.dfields field in
			d.dvalues.(idx) <- v;
			if not (tsame d.dtypes.(idx) vt) then begin
				d.dtypes.(idx) <- vt;
				rebuild_virtuals ctx d;
			end;
		with Not_found ->
			let idx = Array.length d.dvalues in
			Hashtbl.add d.dfields field idx;
			let vals2 = Array.make (idx + 1) VNull in
			let types2 = Array.make (idx + 1) HVoid in
			Array.blit d.dvalues 0 vals2 0 idx;
			Array.blit d.dtypes 0 types2 0 idx;
			vals2.(idx) <- v;
			types2.(idx) <- vt;
			d.dvalues <- vals2;
			d.dtypes <- types2;
			rebuild_virtuals ctx d;
		)
	| VObj o ->
		(try
			let idx, t = get_index field o.oproto.pclass in
			if idx < 0 then raise Not_found;
			o.ofields.(idx) <- dyn_cast ctx v vt t
		with Not_found ->
			throw_msg ctx (o.oproto.pclass.pname ^ " has no field " ^ field))
	| VVirtual vp ->
		dyn_set_field ctx vp.vvalue field v vt
	| VNull ->
		null_access()
	| _ ->
		throw_msg ctx "Invalid object access"

let make_stack ctx (f,pos) =
	let pos = !pos - 1 in
	try let fid, line = f.debug.(pos) in ctx.code.debugfiles.(fid), line with _ -> "???", 0

let stack_frame ctx (f,pos) =
	let file, line = make_stack ctx (f,pos) in
	Printf.sprintf "%s:%d: Called from fun@%d @x%X" file line f.findex (!pos - 1)

let cached_string ctx idx =
	try
		Hashtbl.find ctx.cached_strings idx
	with Not_found ->
		let s = caml_to_hl ctx.code.strings.(idx) in
		Hashtbl.add ctx.cached_strings idx s;
		s

let virt_make_val v =
	let hfields = Hashtbl.create 0 in
	let ftypes = DynArray.create() in
	let values = DynArray.create() in
	Array.iteri (fun i idx ->
		match idx with
		| VFNone -> ()
		| VFIndex k ->
			let n, _, t = v.vtype.vfields.(i) in
			Hashtbl.add hfields n (DynArray.length values);
			DynArray.add values v.vtable.(k);
			DynArray.add ftypes t;
	) v.vindexes;
	VDynObj {
		dfields = hfields;
		dtypes = DynArray.to_array ftypes;
		dvalues = DynArray.to_array values;
		dvirtuals = [v];
	}

let rec vstr ctx v t =
	let vstr = vstr ctx in
	match v with
	| VNull -> "null"
	| VInt i -> Int32.to_string i
	| VInt64 i -> Int64.to_string i
	| VFloat f -> float_to_string f
	| VBool b -> if b then "true" else "false"
	| VDyn (v,t) ->
		vstr v t
	| VObj o ->
		(match get_to_string ctx o.oproto.pclass with
		| None -> o.oproto.pclass.pname
		| Some f -> vstr (ctx.fcall f [v]) HBytes)
	| VBytes b -> (try hl_to_caml b with _ -> "?" ^ String.escaped b)
	| VClosure (f,_) -> fstr f
	| VArray (a,t) -> "[" ^ String.concat ", " (Array.to_list (Array.map (fun v -> vstr v t) a)) ^ "]"
	| VUndef -> "undef"
	| VType t -> tstr t
	| VRef (r,t) -> "*" ^ (vstr (get_ref ctx r) t)
	| VVirtual v ->
		(match v.vvalue with
		| VNull ->
			vstr (virt_make_val v) HDyn
		| _ ->
			vstr v.vvalue HDyn)
	| VDynObj d ->
		(try
			let fid = Hashtbl.find d.dfields "__string" in
			(match d.dtypes.(fid) with HFun ([],HBytes) -> () | _ -> raise Not_found);
			vstr (dyn_call ctx d.dvalues.(fid) [] HBytes) HBytes
		with Not_found ->
			"{" ^ String.concat ", " (Hashtbl.fold (fun f i acc -> (f^":"^vstr d.dvalues.(i) d.dtypes.(i)) :: acc) d.dfields []) ^ "}")
	| VAbstract _ -> "abstract"
	| VEnum (e,i,vals) ->
		let n, _, pl = e.efields.(i) in
		if Array.length pl = 0 then
			n
		else
			let rec loop i =
				if i = Array.length pl then []
				else let v = vals.(i) in vstr v pl.(i) :: loop (i + 1)
			in
			n ^ "(" ^ String.concat "," (loop 0) ^ ")"
	| VVarArgs _ -> "varargs"
	| VStruct s -> "@" ^ s.oproto.pclass.pname

let interp ctx f args =
	let func = get_function ctx in
	let spos = ctx.stack_pos in
	if spos + Array.length f.regs > Array.length ctx.stack then begin
		let nsize = spos + Array.length f.regs + 256 in
		let nstack = Array.make nsize VUndef in
		Array.blit ctx.stack 0 nstack 0 ctx.stack_pos;
		ctx.stack <- nstack;
	end;
	if ctx.checked then for i = 0 to Array.length f.regs - 1 do ctx.stack.(i + spos) <- VUndef; done;
	ctx.stack_pos <- spos + Array.length f.regs;

	let pos = ref 1 in
	ctx.call_stack <- (f,pos) :: ctx.call_stack;
	let fret = (match f.ftype with
		| HFun (fargs,fret) ->
			if ctx.checked && List.length fargs <> List.length args then error (Printf.sprintf "Invalid args: (%s) should be (%s)" (String.concat "," (List.map (vstr_d ctx) args)) (String.concat "," (List.map tstr fargs)));
			fret
		| _ -> Globals.die "" __LOC__
	) in
	let fcall = ctx.fcall in
	let rtype i = Array.unsafe_get f.regs i in
	let check v t id =
		if ctx.checked && not (is_compatible v t) then error (Printf.sprintf "Can't set %s(%s) with %s" (id()) (tstr t) (vstr_d ctx v))
	in
	let check_obj v o fid =
		if ctx.checked then match o with
		| VObj o ->
			let _, fields, _ = get_proto ctx o.oproto.pclass in
			check v fields.(fid) (fun() -> "obj field")
		| VVirtual vp ->
			let _,_, t = vp.vtype.vfields.(fid) in
			check v t (fun() -> "virtual field")
		| _ ->
			()
	in
	let set r v =
		check v (rtype r) (fun() -> "register " ^ string_of_int r);
		Array.unsafe_set ctx.stack (r + spos) v
	in
	list_iteri set args;
	let get r = Array.unsafe_get ctx.stack (r + spos) in
	let global g = Array.unsafe_get ctx.t_globals g in
	let traps = ref [] in
	let numop iop fop a b =
		match rtype a with
		(* todo : sign-extend and mask after result for HUI8/16 *)
		| HUI8 | HUI16 | HI32 ->
			(match get a, get b with
			| VInt a, VInt b -> VInt (iop a b)
			| _ -> Globals.die "" __LOC__)
		| HF32 | HF64 ->
			(match get a, get b with
			| VFloat a, VFloat b -> VFloat (fop a b)
			| _ -> Globals.die "" __LOC__)
		| _ ->
			Globals.die "" __LOC__
	in
	let iop f a b =
		match rtype a with
		(* todo : sign-extend and mask after result for HUI8/16 *)
		| HUI8 | HUI16 | HI32 ->
			(match get a, get b with
			| VInt a, VInt b -> VInt (f a b)
			| _ -> Globals.die "" __LOC__)
		| _ ->
			Globals.die "" __LOC__
	in
	let iunop iop r =
		match rtype r with
		| HUI8 | HUI16 | HI32 ->
			(match get r with
			| VInt a -> VInt (iop a)
			| _ -> Globals.die "" __LOC__)
		| _ ->
			Globals.die "" __LOC__
	in
	let ucompare a b =
		match a, b with
		| VInt a, VInt b ->
			let d = Int32.sub (Int32.shift_right_logical a 16) (Int32.shift_right_logical b 16) in
			Int32.to_int (if d = 0l then Int32.sub (Int32.logand a 0xFFFFl) (Int32.logand b 0xFFFFl) else d)
		| _ -> Globals.die "" __LOC__
	in
	let vcompare ra rb op =
		let a = get ra in
		let b = get rb in
		let t = rtype ra in
		let r = dyn_compare ctx a t b t in
		if r = invalid_comparison then false else op r 0
	in
	let ufloat v =
		if v < 0l then Int32.to_float v +. 4294967296. else Int32.to_float v
	in
	let rec loop() =
		let op = Array.unsafe_get f.code (!pos) in
		incr pos;
		(match op with
		| OMov (a,b) -> set a (get b)
		| OInt (r,i) -> set r (VInt ctx.code.ints.(i))
		| OFloat (r,i) -> set r (VFloat (Array.unsafe_get ctx.code.floats i))
		| OString (r,s) -> set r (VBytes (cached_string ctx s))
		| OBytes (r,s) -> set r (VBytes (Bytes.to_string ctx.code.bytes.(s)))
		| OBool (r,b) -> set r (VBool b)
		| ONull r -> set r VNull
		| OAdd (r,a,b) -> set r (numop Int32.add ( +. ) a b)
		| OSub (r,a,b) -> set r (numop Int32.sub ( -. ) a b)
		| OMul (r,a,b) -> set r (numop Int32.mul ( *. ) a b)
		| OSDiv (r,a,b) -> set r (numop (fun a b -> if b = 0l then 0l else Int32.div a b) ( /. ) a b)
		| OUDiv (r,a,b) -> set r (iop (fun a b -> if b = 0l then 0l else Int32.of_float ((ufloat a) /. (ufloat b))) a b)
		| OSMod (r,a,b) -> set r (numop (fun a b -> if b = 0l then 0l else Int32.rem a b) mod_float a b)
		| OUMod (r,a,b) -> set r (iop (fun a b -> if b = 0l then 0l else Int32.of_float (mod_float (ufloat a) (ufloat b))) a b)
		| OShl (r,a,b) -> set r (iop (fun a b -> Int32.shift_left a (Int32.to_int b)) a b)
		| OSShr (r,a,b) -> set r (iop (fun a b -> Int32.shift_right a (Int32.to_int b)) a b)
		| OUShr (r,a,b) -> set r (iop (fun a b -> Int32.shift_right_logical a (Int32.to_int b)) a b)
		| OAnd (r,a,b) -> set r (iop Int32.logand a b)
		| OOr (r,a,b) -> set r (iop Int32.logor a b)
		| OXor (r,a,b) -> set r (iop Int32.logxor a b)
		| ONeg (r,v) -> set r (match get v with VInt v -> VInt (Int32.neg v) | VFloat f -> VFloat (-. f) | _ -> Globals.die "" __LOC__)
		| ONot (r,v) -> set r (match get v with VBool b -> VBool (not b) | _ -> Globals.die "" __LOC__)
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
			check v ctx.code.globals.(g) (fun() -> "global " ^ string_of_int g);
			Array.unsafe_set ctx.t_globals g v
		| OJTrue (r,i) -> if get r = VBool true then pos := !pos + i
		| OJFalse (r,i) -> if get r = VBool false then pos := !pos + i
		| ORet r -> raise (Return (get r))
		| OJNull (r,i) -> if get r == VNull then pos := !pos + i
		| OJNotNull (r,i) -> if get r != VNull then pos := !pos + i
		| OJSLt (a,b,i) -> if vcompare a b (<) then pos := !pos + i
		| OJSGte (a,b,i) -> if vcompare a b (>=) then pos := !pos + i
		| OJSGt (a,b,i) -> if vcompare a b (>) then pos := !pos + i
		| OJSLte (a,b,i) -> if vcompare a b (<=) then pos := !pos + i
		| OJULt (a,b,i) -> if ucompare (get a) (get b) < 0 then pos := !pos + i
		| OJUGte (a,b,i) -> if ucompare (get a) (get b) >= 0 then pos := !pos + i
		| OJNotLt (a,b,i) -> if not (vcompare a b (<)) then pos := !pos + i
		| OJNotGte (a,b,i) -> if not (vcompare a b (>=)) then pos := !pos + i
		| OJEq (a,b,i) -> if vcompare a b (=) then pos := !pos + i
		| OJNotEq (a,b,i) -> if not (vcompare a b (=)) then pos := !pos + i
		| OJAlways i -> pos := !pos + i
		| OToDyn (r,a) -> set r (make_dyn (get a) f.regs.(a))
		| OToSFloat (r,a) -> set r (match get a with VInt v -> VFloat (Int32.to_float v) | VFloat _ as v -> v | _ -> Globals.die "" __LOC__)
		| OToUFloat (r,a) -> set r (match get a with VInt v -> VFloat (ufloat v) | VFloat _ as v -> v | _ -> Globals.die "" __LOC__)
		| OToInt (r,a) -> set r (match get a with VFloat v -> VInt (Int32.of_float v) | VInt i when rtype r = HI64 -> VInt64 (Int64.of_int32 i) | VInt _ as v -> v | _ -> Globals.die "" __LOC__)
		| OLabel _ -> ()
		| ONew r ->
			set r (alloc_obj ctx (rtype r))
		| OField (r,o,fid) ->
			set r (match get o with
				| VObj v | VStruct v -> v.ofields.(fid)
				| VVirtual v as obj ->
					(match v.vindexes.(fid) with
					| VFNone -> dyn_get_field ctx obj (let n,_,_ = v.vtype.vfields.(fid) in n) (rtype r)
					| VFIndex i -> v.vtable.(i))
				| VNull -> null_access()
				| _ -> Globals.die "" __LOC__)
		| OSetField (o,fid,r) ->
			let rv = get r in
			let o = get o in
			(match o with
			| VObj v | VStruct v ->
				check_obj rv o fid;
				v.ofields.(fid) <- rv
			| VVirtual v ->
				(match v.vindexes.(fid) with
				| VFNone ->
					dyn_set_field ctx o (let n,_,_ = v.vtype.vfields.(fid) in n) rv (rtype r)
				| VFIndex i ->
					check_obj rv o fid;
					v.vtable.(i) <- rv)
			| VNull -> null_access()
			| _ -> Globals.die "" __LOC__)
		| OGetThis (r, fid) ->
			set r (match get 0 with VObj v | VStruct v -> v.ofields.(fid) | _ -> Globals.die "" __LOC__)
		| OSetThis (fid, r) ->
			(match get 0 with
			| (VObj v | VStruct v) as o ->
				let rv = get r in
				check_obj rv o fid;
				v.ofields.(fid) <- rv
			| _ -> Globals.die "" __LOC__)
		| OCallMethod (r,m,rl) ->
			(match get (List.hd rl) with
			| VObj v -> set r (fcall v.oproto.pmethods.(m) (List.map get rl))
			| VVirtual v ->
				let name, _, _ = v.vtype.vfields.(m) in
				(match v.vvalue with
				| VObj o as obj ->
					(try
						let m = PMap.find name o.oproto.pclass.pfunctions in
						set r (dyn_call ctx (VClosure (get_function ctx m,Some obj)) (List.map (fun r -> get r, rtype r) (List.tl rl)) (rtype r))
					with Not_found ->
						Globals.die "" __LOC__)
				| VDynObj _ ->
					set r (dyn_call ctx v.vvalue (List.map (fun r -> get r, rtype r) (List.tl rl)) (rtype r))
				| _ ->
					Globals.die "" __LOC__)
			| VNull -> null_access()
			| _ -> Globals.die "" __LOC__)
		| OCallThis (r,m,rl) ->
			(match get 0 with
			| VObj v as o -> set r (fcall v.oproto.pmethods.(m) (o :: List.map get rl))
			| _ -> Globals.die "" __LOC__)
		| OCallClosure (r,v,rl) ->
			if rtype v = HDyn then
				set r (dyn_call ctx (get v) (List.map (fun r -> get r, rtype r) rl) (rtype r))
			else (match get v with
			| VClosure (f,None) -> set r (fcall f (List.map get rl))
			| VClosure (f,Some arg) -> set r (fcall f (arg :: List.map get rl))
			| VNull -> null_access()
			| _ -> throw_msg ctx (vstr_d ctx (get v)))
		| OStaticClosure (r, fid) ->
			let f = get_function ctx fid in
			set r (VClosure (f,None))
		| OInstanceClosure (r, fid, v) ->
			let f = get_function ctx fid in
			set r (VClosure (f,Some (get v)))
		| OVirtualClosure (r, o, m) ->
			let m = (match get o with
			| VObj v as obj -> VClosure (v.oproto.pmethods.(m), Some obj)
			| VNull -> null_access()
			| _ -> Globals.die "" __LOC__
			) in
			set r (if m = VNull then m else dyn_cast ctx m (match get_type m with None -> Globals.die "" __LOC__ | Some v -> v) (rtype r))
		| OThrow r ->
			throw ctx (get r)
		| ORethrow r ->
			ctx.call_stack <- List.rev ctx.error_stack @ ctx.call_stack;
			throw ctx (get r)
		| OGetUI8 (r,b,p) ->
			(match get b, get p with
			| VBytes b, VInt p -> set r (VInt (Int32.of_int (int_of_char (String.get b (Int32.to_int p)))))
			| _ -> Globals.die "" __LOC__)
		| OGetUI16 (r,b,p) ->
			(match get b, get p with
			| VBytes b, VInt p ->
				let a = int_of_char (String.get b (Int32.to_int p)) in
				let b = int_of_char (String.get b (Int32.to_int p + 1)) in
				set r (VInt (Int32.of_int (a lor (b lsl 8))))
			| _ -> Globals.die "" __LOC__)
		| OGetMem (r,b,p) ->
			(match get b, get p with
			| VBytes b, VInt p ->
				let p = Int32.to_int p in
				set r (match rtype r with
				| HI32 -> VInt (get_i32 b p)
				| HI64 -> VInt64 (get_i64 b p)
				| HF32 -> VFloat (Int32.float_of_bits (get_i32 b p))
				| HF64 -> VFloat (Int64.float_of_bits (get_i64 b p))
				| _ -> Globals.die "" __LOC__)
			| _ ->
				Globals.die "" __LOC__)
		| OGetArray (r,a,i) ->
			(match get a, get i with
			| VArray (a,_), VInt i -> set r a.(Int32.to_int i)
			| _ -> Globals.die "" __LOC__);
		| OSetUI8 (r,p,v) ->
			(match get r, get p, get v with
			| VBytes b, VInt p, VInt v -> Bytes.set (Bytes.unsafe_of_string b) (Int32.to_int p) (char_of_int ((Int32.to_int v) land 0xFF))
			| _ -> Globals.die "" __LOC__)
		| OSetUI16 (r,p,v) ->
			(match get r, get p, get v with
			| VBytes b, VInt p, VInt v ->
				Bytes.set (Bytes.unsafe_of_string b) (Int32.to_int p) (char_of_int ((Int32.to_int v) land 0xFF));
				Bytes.set (Bytes.unsafe_of_string b) (Int32.to_int p + 1) (char_of_int (((Int32.to_int v) lsr 8) land 0xFF))
			| _ -> Globals.die "" __LOC__)
		| OSetMem (r,p,v) ->
			(match get r, get p with
			| VBytes b, VInt p ->
				let p = Int32.to_int p in
				(match rtype v, get v with
				| HI32, VInt v -> set_i32 b p v
				| HI64, VInt64 v -> set_i64 b p v
				| HF32, VFloat f -> set_i32 b p (Int32.bits_of_float f)
				| HF64, VFloat f -> set_i64 b p (Int64.bits_of_float f)
				| _ -> Globals.die "" __LOC__)
			| _ ->
				Globals.die "" __LOC__)
		| OSetArray (a,i,v) ->
			(match get a, get i with
			| VArray (a,t), VInt i ->
				let v = get v in
				check v t (fun() -> "array");
				let idx = Int32.to_int i in
				if ctx.checked && (idx < 0 || idx >= Array.length a) then error (Printf.sprintf "Can't set array index %d with %s" idx (vstr_d ctx v));
				a.(Int32.to_int i) <- v
			| _ -> Globals.die "" __LOC__);
		| OSafeCast (r, v) ->
			set r (dyn_cast ctx (get v) (rtype v) (rtype r))
		| OUnsafeCast (r,v) ->
			set r (get v)
		| OArraySize (r,a) ->
			(match get a with
			| VArray (a,_) -> set r (VInt (Int32.of_int (Array.length a)));
			| _ -> Globals.die "" __LOC__)
		| OType (r,t) ->
			set r (VType t)
		| OGetType (r,v) ->
			let v = get v in
			let v = (match v with VVirtual { vvalue = VNull } -> Globals.die "" __LOC__ | VVirtual v -> v.vvalue | _ -> v) in
			set r (VType (if v = VNull then HVoid else match get_type v with None -> Globals.die "" __LOC__ | Some t -> t));
		| OGetTID (r,v) ->
			set r (match get v with
				| VType t ->
					(VInt (Int32.of_int (match t with
					| HVoid -> 0
					| HUI8 -> 1
					| HUI16 -> 2
					| HI32 -> 3
					| HI64 -> 4
					| HF32 -> 5
					| HF64 -> 6
					| HBool -> 7
					| HBytes -> 8
					| HDyn -> 9
					| HFun _ -> 10
					| HObj _ -> 11
					| HArray -> 12
					| HType -> 13
					| HRef _ -> 14
					| HVirtual _ -> 15
					| HDynObj -> 16
					| HAbstract _ -> 17
					| HEnum _ -> 18
					| HNull _ -> 19
					| HMethod _ -> 20
					| HStruct _ -> 21
					| HPacked _ -> 22)))
				| _ -> Globals.die "" __LOC__);
		| ORef (r,v) ->
			set r (VRef (RStack (v + spos),rtype v))
		| OUnref (v,r) ->
			set v (match get r with
			| VRef (r,_) -> get_ref ctx r
			| _ -> Globals.die "" __LOC__)
		| OSetref (r,v) ->
			(match get r with
			| VRef (r,t) ->
				let v = get v in
				check v t (fun() -> "ref");
				set_ref ctx r v
			| _ -> Globals.die "" __LOC__)
		| OToVirtual (r,rv) ->
			set r (to_virtual ctx (get rv) (match rtype r with HVirtual vp -> vp | _ -> Globals.die "" __LOC__))
		| ODynGet (r,o,f) ->
			set r (dyn_get_field ctx (get o) ctx.code.strings.(f) (rtype r))
		| ODynSet (o,fid,vr) ->
			dyn_set_field ctx (get o) ctx.code.strings.(fid) (get vr) (rtype vr)
		| OMakeEnum (r,e,pl) ->
			set r (VEnum ((match rtype r with HEnum e -> e | _ -> Globals.die "" __LOC__),e,Array.map get (Array.of_list pl)))
		| OEnumAlloc (r,f) ->
			(match rtype r with
			| HEnum e ->
				let _, _, fl = e.efields.(f) in
				let vl = Array.make (Array.length fl) VUndef in
				set r (VEnum (e, f, vl))
			| _ -> Globals.die "" __LOC__
			)
		| OEnumIndex (r,v) ->
			(match get v with
			| VEnum (_,i,_) -> set r (VInt (Int32.of_int i))
			| _ -> Globals.die "" __LOC__)
		| OEnumField (r, v, _, i) ->
			(match get v with
			| VEnum (_,_,vl) -> set r vl.(i)
			| _ -> Globals.die "" __LOC__)
		| OSetEnumField (v, i, r) ->
			(match get v, rtype v with
			| VEnum (_,id,vl), HEnum e ->
				let rv = get r in
				let _, _, fields = e.efields.(id) in
				check rv fields.(i) (fun() -> "enumfield");
				vl.(i) <- rv
			| _ -> Globals.die "" __LOC__)
		| OSwitch (r, indexes, _) ->
			(match get r with
			| VInt i ->
				let i = Int32.to_int i in
				if i >= 0 && i < Array.length indexes then pos := !pos + indexes.(i)
			| _ -> Globals.die "" __LOC__)
		| ONullCheck r ->
			if get r = VNull then throw_msg ctx "Null access"
		| OTrap (r,j) ->
			let target = !pos + j in
			traps := (r,target) :: !traps
		| OEndTrap _ ->
			traps := List.tl !traps
		| OAssert _ ->
			throw_msg ctx "Assert"
		| ORefData (r,d) ->
			(match get d with
			| VArray (a,t) -> set r (VRef (RArray (a,0),t))
			| _ -> Globals.die "" __LOC__)
		| ORefOffset (r,r2,off) ->
			(match get r2, get off with
			| VRef (RArray (a,pos),t), VInt i -> set r (VRef (RArray (a,pos + Int32.to_int i),t))
			| _ -> Globals.die "" __LOC__)
		| ONop _ ->
			()
		);
		loop()
	in
	let rec exec() =
		try
			loop()
		with
			| Return v ->
				check v fret (fun() -> "return value");
				ctx.call_stack <- List.tl ctx.call_stack;
				ctx.stack_pos <- spos;
				v
			| InterpThrow v ->
				match !traps with
				| [] ->
					ctx.error_stack <- List.hd ctx.call_stack :: ctx.error_stack;
					ctx.call_stack <- List.tl ctx.call_stack;
					raise (InterpThrow v)
				| (r,target) :: tl ->
					traps := tl;
					ctx.error_stack <- (f,ref !pos) :: ctx.error_stack;
					pos := target;
					ctx.stack_pos <- spos + Array.length f.regs;
					set r v;
					exec()
	in
	pos := 0;
	exec()


let call_fun ctx f args =
	match f with
	| FFun f -> interp ctx f args
	| FNativeFun (_,f,_) ->
		try
			f args
		with InterpThrow v ->
			raise (InterpThrow v)
		| Failure msg ->
			throw_msg ctx msg
		| Sys_exit _ as exc ->
			raise exc
(*		| e ->
			throw_msg ctx (Printexc.to_string e)
*)

let call_wrap ?(final=(fun()->())) ctx f args =
	let old_st = ctx.call_stack in
	let old_pos = ctx.stack_pos in
	let restore() =
		ctx.call_stack <- old_st;
		ctx.stack_pos <- old_pos;
	in
	try
		let v = call_fun ctx f args in
		final();
		v
	with
		| InterpThrow v ->
			restore();
			final();
			ctx.on_error v (List.rev ctx.error_stack);
			VNull
		| Runtime_error msg ->
			let stack = ctx.call_stack in
			restore();
			final();
			ctx.on_error (VBytes (caml_to_hl ("HL Interp error " ^ msg))) stack;
			VNull

(* ------------------------------- HL RUNTIME ---------------------------------------------- *)

let load_native ctx lib name t =
	let unresolved() = (fun args -> error ("Unresolved native " ^ lib ^ "@" ^ name)) in
	let int = Int32.to_int in
	let to_int i = VInt (Int32.of_int i) in
	let date d =
		Unix.localtime (Int32.to_float d)
	in
	let to_date d =
		let t, _ = Unix.mktime d in
		VInt (Int32.of_float t)
	in
	let hl_to_caml_sub str pos len =
		hl_to_caml (String.sub str pos len ^ "\x00\x00")
	in
	let no_virtual v =
		match v with
		| VVirtual v when v.vvalue <> VNull -> v.vvalue
		| _ -> v
	in
	let set_ref = set_ref ctx in
	let f = (match lib with
	| "std" ->
		(match name with
		| "alloc_bytes" ->
			(function
			| [VInt i] -> VBytes (Bytes.unsafe_to_string (Bytes.create (int i)))
			| _ -> Globals.die "" __LOC__)
		| "alloc_array" ->
			(function
			| [VType t;VInt i] -> VArray (Array.make (int i) (default t),t)
			| _ -> Globals.die "" __LOC__)
		| "alloc_obj" ->
			(function
			| [VType t] -> alloc_obj ctx t
			| _ -> Globals.die "" __LOC__)
		| "alloc_enum_dyn" ->
			(function
			| [VType (HEnum e); VInt idx; VArray (vl,vt); VInt len] ->
				let idx = int idx in
				let len = int len in
				let _, _, args = e.efields.(idx) in
				if Array.length args <> len then
					VNull
				else
					VEnum (e,idx,Array.mapi (fun i v -> dyn_cast ctx v vt args.(i)) (Array.sub vl 0 len))
			| vl ->
				Globals.die "" __LOC__)
		| "array_blit" ->
			(function
			| [VArray (dst,_); VInt dp; VArray (src,_); VInt sp; VInt len] ->
				Array.blit src (int sp) dst (int dp) (int len);
				VUndef
			| _ -> Globals.die "" __LOC__)
		| "bytes_blit" ->
			(function
			| [VBytes dst; VInt dp; VBytes src; VInt sp; VInt len] ->
				String.blit src (int sp) (Bytes.unsafe_of_string dst) (int dp) (int len);
				VUndef
			| [(VBytes _ | VNull); VInt _; (VBytes _ | VNull); VInt _; VInt len] ->
				if len = 0l then VUndef else error "bytes_blit to NULL bytes";
			| _ -> Globals.die "" __LOC__)
		| "bsort_i32" ->
			(function
			| [VBytes b; VInt pos; VInt len; VClosure (f,c)] ->
				let pos = int pos and len = int len in
				let a = Array.init len (fun i -> get_i32 b (pos + i * 4)) in
				Array.stable_sort (fun a b ->
					match ctx.fcall f (match c with None -> [VInt a;VInt b] | Some v -> [v;VInt a;VInt b]) with
					| VInt i -> int i
					| _ -> Globals.die "" __LOC__
				) a;
				Array.iteri (fun i v -> set_i32 b (pos + i * 4) v) a;
				VUndef;
			| _ ->
				Globals.die "" __LOC__)
		| "bsort_f64" ->
			(function
			| [VBytes b; VInt pos; VInt len; VClosure _] ->
				Globals.die "" __LOC__
			| _ ->
				Globals.die "" __LOC__)
		| "itos" ->
			(function
			| [VInt v; VRef (r,_)] ->
				let str = Int32.to_string v in
				set_ref r (to_int (String.length str));
				VBytes (caml_to_hl str)
			| _ -> Globals.die "" __LOC__);
		| "ftos" ->
			(function
			| [VFloat f; VRef (r,_)] ->
				let str = float_to_string f in
				set_ref r (to_int (String.length str));
				VBytes (caml_to_hl str)
			| _ -> Globals.die "" __LOC__);
		| "value_to_string" ->
			(function
			| [v; VRef (r,_)] ->
				let str = caml_to_hl (vstr ctx v HDyn) in
				set_ref r (to_int ((String.length str) lsr 1 - 1));
				VBytes str
			| _ -> Globals.die "" __LOC__);
		| "math_isnan" -> (function [VFloat f] -> VBool (classify_float f = FP_nan) | _ -> Globals.die "" __LOC__)
		| "math_isfinite" -> (function [VFloat f] -> VBool (match classify_float f with FP_infinite | FP_nan -> false | _ -> true) | _ -> Globals.die "" __LOC__)
		| "math_round" -> (function [VFloat f] -> VInt (Int32.of_float (floor (f +. 0.5))) | _ -> Globals.die "" __LOC__)
		| "math_floor" -> (function [VFloat f] -> VInt (Int32.of_float (floor f)) | _ -> Globals.die "" __LOC__)
		| "math_ceil" -> (function [VFloat f] -> VInt (Int32.of_float (ceil f)) | _ -> Globals.die "" __LOC__)
		| "math_ffloor" -> (function [VFloat f] -> VFloat (floor f) | _ -> Globals.die "" __LOC__)
		| "math_fceil" -> (function [VFloat f] -> VFloat (ceil f) | _ -> Globals.die "" __LOC__)
		| "math_fround" -> (function [VFloat f] -> VFloat (floor (f +. 0.5)) | _ -> Globals.die "" __LOC__)
		| "math_abs" -> (function [VFloat f] -> VFloat (abs_float f) | _ -> Globals.die "" __LOC__)
		| "math_sqrt" -> (function [VFloat f] -> VFloat (if f < 0. then nan else sqrt f) | _ -> Globals.die "" __LOC__)
		| "math_cos" -> (function [VFloat f] -> VFloat (cos f) | _ -> Globals.die "" __LOC__)
		| "math_sin" -> (function [VFloat f] -> VFloat (sin f) | _ -> Globals.die "" __LOC__)
		| "math_tan" -> (function [VFloat f] -> VFloat (tan f) | _ -> Globals.die "" __LOC__)
		| "math_acos" -> (function [VFloat f] -> VFloat (acos f) | _ -> Globals.die "" __LOC__)
		| "math_asin" -> (function [VFloat f] -> VFloat (asin f) | _ -> Globals.die "" __LOC__)
		| "math_atan" -> (function [VFloat f] -> VFloat (atan f) | _ -> Globals.die "" __LOC__)
		| "math_atan2" -> (function [VFloat a; VFloat b] -> VFloat (atan2 a b) | _ -> Globals.die "" __LOC__)
		| "math_log" -> (function [VFloat f] -> VFloat (Stdlib.log f) | _ -> Globals.die "" __LOC__)
		| "math_exp" -> (function [VFloat f] -> VFloat (exp f) | _ -> Globals.die "" __LOC__)
		| "math_pow" -> (function [VFloat a; VFloat b] -> VFloat (a ** b) | _ -> Globals.die "" __LOC__)
		| "parse_int" ->
			(function
			| [VBytes str; VInt pos; VInt len] ->
				(try
					VDyn (VInt (Numeric.parse_int (hl_to_caml_sub str (int pos) (int len))),HI32)
				with _ ->
					VNull)
			| l -> Globals.die "" __LOC__)
		| "parse_float" ->
			(function
			| [VBytes str; VInt pos; VInt len] -> (try VFloat (Numeric.parse_float (hl_to_caml_sub str (int pos) (int len))) with _ -> VFloat nan)
			| _ -> Globals.die "" __LOC__)
		| "dyn_compare" ->
			(function
			| [a;b] -> to_int (dyn_compare ctx a HDyn b HDyn)
			| _ -> Globals.die "" __LOC__)
		| "fun_compare" ->
			let ocompare o1 o2 =
				match o1, o2 with
				| None, None -> true
				| Some o1, Some o2 -> o1 == o2
				| _ -> false
			in
			(function
			| [VClosure (FFun f1,o1);VClosure (FFun f2,o2)] -> VBool (f1 == f2 && ocompare o1 o2)
			| [VClosure (FNativeFun (f1,_,_),o1);VClosure (FNativeFun (f2,_,_),o2)] -> VBool (f1 = f2 && ocompare o1 o2)
			| _ -> VBool false)
		| "array_type" ->
			(function
			| [VArray (_,t)] -> VType t
			| _ -> Globals.die "" __LOC__)
		| "value_cast" ->
			(function
			| [v;VType t] -> if is_compatible v t then v else throw_msg ctx ("Cannot cast " ^ vstr_d ctx v ^ " to " ^ tstr t);
			| _ -> Globals.die "" __LOC__)
		| "hballoc" ->
			(function
			| [] -> VAbstract (AHashBytes (Hashtbl.create 0))
			| _ -> Globals.die "" __LOC__)
		| "hbset" ->
			(function
			| [VAbstract (AHashBytes h);VBytes b;v] ->
				Hashtbl.replace h (hl_to_caml b) v;
				VUndef
			| _ -> Globals.die "" __LOC__)
		| "hbget" ->
			(function
			| [VAbstract (AHashBytes h);VBytes b] ->
				(try Hashtbl.find h (hl_to_caml b) with Not_found -> VNull)
			| _ -> Globals.die "" __LOC__)
		| "hbvalues" ->
			(function
			| [VAbstract (AHashBytes h)] ->
				let values = Hashtbl.fold (fun _ v acc -> v :: acc) h [] in
				VArray (Array.of_list values, HDyn)
			| _ -> Globals.die "" __LOC__)
		| "hbkeys" ->
			(function
			| [VAbstract (AHashBytes h)] ->
				let keys = Hashtbl.fold (fun s _ acc -> VBytes (caml_to_hl s) :: acc) h [] in
				VArray (Array.of_list keys, HBytes)
			| _ -> Globals.die "" __LOC__)
		| "hbexists" ->
			(function
			| [VAbstract (AHashBytes h);VBytes b] -> VBool (Hashtbl.mem h (hl_to_caml b))
			| _ -> Globals.die "" __LOC__)
		| "hbremove" ->
			(function
			| [VAbstract (AHashBytes h);VBytes b] ->
				let m = Hashtbl.mem h (hl_to_caml b) in
				if m then Hashtbl.remove h (hl_to_caml b);
				VBool m
			| _ -> Globals.die "" __LOC__)
		| "hialloc" ->
			(function
			| [] -> VAbstract (AHashInt (Hashtbl.create 0))
			| _ -> Globals.die "" __LOC__)
		| "hiset" ->
			(function
			| [VAbstract (AHashInt h);VInt i;v] ->
				Hashtbl.replace h i v;
				VUndef
			| _ -> Globals.die "" __LOC__)
		| "higet" ->
			(function
			| [VAbstract (AHashInt h);VInt i] ->
				(try Hashtbl.find h i with Not_found -> VNull)
			| _ -> Globals.die "" __LOC__)
		| "hivalues" ->
			(function
			| [VAbstract (AHashInt h)] ->
				let values = Hashtbl.fold (fun _ v acc -> v :: acc) h [] in
				VArray (Array.of_list values, HDyn)
			| _ -> Globals.die "" __LOC__)
		| "hikeys" ->
			(function
			| [VAbstract (AHashInt h)] ->
				let keys = Hashtbl.fold (fun i _ acc -> VInt i :: acc) h [] in
				VArray (Array.of_list keys, HI32)
			| _ -> Globals.die "" __LOC__)
		| "hiexists" ->
			(function
			| [VAbstract (AHashInt h);VInt i] -> VBool (Hashtbl.mem h i)
			| _ -> Globals.die "" __LOC__)
		| "hiremove" ->
			(function
			| [VAbstract (AHashInt h);VInt i] ->
				let m = Hashtbl.mem h i in
				if m then Hashtbl.remove h i;
				VBool m
			| _ -> Globals.die "" __LOC__)
		| "hoalloc" ->
			(function
			| [] -> VAbstract (AHashObject (ref []))
			| _ -> Globals.die "" __LOC__)
		| "hoset" ->
			(function
			| [VAbstract (AHashObject l);o;v] ->
				let o = no_virtual o in
				let rec replace l =
					match l with
					| [] -> [o,v]
					| (o2,_) :: l when o == o2 -> (o,v) :: l
					| p :: l -> p :: replace l
				in
				l := replace !l;
				VUndef
			| _ -> Globals.die "" __LOC__)
		| "hoget" ->
			(function
			| [VAbstract (AHashObject l);o] ->
				(try List.assq (no_virtual o) !l with Not_found -> VNull)
			| _ -> Globals.die "" __LOC__)
		| "hovalues" ->
			(function
			| [VAbstract (AHashObject l)] ->
				VArray (Array.of_list (List.map snd !l), HDyn)
			| _ -> Globals.die "" __LOC__)
		| "hokeys" ->
			(function
			| [VAbstract (AHashObject l)] ->
				VArray (Array.of_list (List.map fst !l), HDyn)
			| _ -> Globals.die "" __LOC__)
		| "hoexists" ->
			(function
			| [VAbstract (AHashObject l);o] -> VBool (List.mem_assq (no_virtual o) !l)
			| _ -> Globals.die "" __LOC__)
		| "horemove" ->
			(function
			| [VAbstract (AHashObject rl);o] ->
				let rec loop acc = function
					| [] -> false
					| (o2,_) :: l when o == o2 ->
						rl := (List.rev acc) @ l;
						true
					| p :: l -> loop (p :: acc) l
				in
				VBool (loop [] !rl)
			| _ -> Globals.die "" __LOC__)
		| "sys_print" ->
			(function
			| [VBytes str] -> print_string (hl_to_caml str); VUndef
			| _ -> Globals.die "" __LOC__)
		| "sys_time" ->
			(function
			| [] -> VFloat (Unix.gettimeofday())
			| _ -> Globals.die "" __LOC__)
		| "sys_exit" ->
			(function
			| [VInt code] -> raise (Sys_exit (Int32.to_int code))
			| _ -> Globals.die "" __LOC__)
		| "sys_utf8_path" ->
			(function
			| [] -> VBool true
			| _ -> Globals.die "" __LOC__)
		| "sys_string" ->
			let cached_sys_name = ref None in
			(function
			| [] ->
				VBytes (caml_to_hl (match Sys.os_type with
				| "Unix" ->
					(match !cached_sys_name with
					| Some n -> n
					| None ->
						let ic, pid = Process_helper.open_process_args_in_pid "uname" [| "uname" |] in
						let uname = (match input_line ic with
							| "Darwin" -> "Mac"
							| n -> n
						) in
						Stdlib.ignore (Process_helper.close_process_in_pid (ic, pid));
						cached_sys_name := Some uname;
						uname)
				| "Win32" | "Cygwin" -> "Windows"
				| s -> s))
			| _ ->
				Globals.die "" __LOC__)
		| "sys_is64" ->
			(function
			| [] -> VBool (Sys.word_size = 64)
			| _ -> Globals.die "" __LOC__)
		| "hash" ->
			(function
			| [VBytes str] -> VInt (hash ctx (hl_to_caml str))
			| _ -> Globals.die "" __LOC__)
		| "type_safe_cast" ->
			(function
			| [VType a; VType b] -> VBool (safe_cast a b)
			| _ -> Globals.die "" __LOC__)
		| "type_super" ->
			(function
			| [VType t] -> VType (match t with HObj { psuper = Some o } -> HObj o | _ -> HVoid)
			| _ -> Globals.die "" __LOC__)
		| "type_args_count" ->
			(function
			| [VType t] -> to_int (match t with HFun (args,_) -> List.length args | _ -> 0)
			| _ -> Globals.die "" __LOC__)
		| "type_get_global" ->
			(function
			| [VType t] ->
				(match t with
				| HObj c -> (match c.pclassglobal with None -> VNull | Some g -> ctx.t_globals.(g))
				| HEnum e -> (match e.eglobal with None -> VNull | Some g -> ctx.t_globals.(g))
				| _ -> VNull)
			| _ -> Globals.die "" __LOC__)
		| "type_set_global" ->
			(function
			| [VType t; v] ->
				VBool (match t with
				| HObj c -> (match c.pclassglobal with None -> false | Some g -> ctx.t_globals.(g) <- v; true)
				| HEnum e -> (match e.eglobal with None -> false | Some g -> ctx.t_globals.(g) <- v; true)
				| _ -> false)
			| _ -> Globals.die "" __LOC__)
		| "type_name" ->
			(function
			| [VType t] ->
				VBytes (caml_to_hl (match t with
				| HObj o -> o.pname
				| HEnum e -> e.ename
				| _ -> Globals.die "" __LOC__))
			| _ -> Globals.die "" __LOC__)
		| "obj_fields" ->
			let rec get_fields v isRec =
				match v with
				| VDynObj o ->
					VArray (Array.of_list (Hashtbl.fold (fun n _ acc -> VBytes (caml_to_hl n) :: acc) o.dfields []), HBytes)
				| VObj o ->
					let rec loop p =
						let fields = Array.map (fun (n,_,_) -> VBytes (caml_to_hl n)) p.pfields in
						match p.psuper with Some p when isRec -> fields :: loop p | _ -> [fields]
					in
					VArray (Array.concat (loop o.oproto.pclass), HBytes)
				| VVirtual v ->
					get_fields v.vvalue isRec
				| _ ->
					VNull
			in
			(function
			| [v] -> get_fields v true
			| _ -> Globals.die "" __LOC__)
		| "obj_copy" ->
			(function
			| [VDynObj d | VVirtual { vvalue = VDynObj d }] ->
				VDynObj { dfields = Hashtbl.copy d.dfields; dvalues = Array.copy d.dvalues; dtypes = Array.copy d.dtypes; dvirtuals = [] }
			| [_] -> VNull
			| _ -> Globals.die "" __LOC__)
		| "enum_parameters" ->
			(function
			| [VEnum (e,idx,pl)] ->
				let _,_, ptypes = e.efields.(idx) in
				VArray (Array.mapi (fun i v -> make_dyn v ptypes.(i)) pl,HDyn)
			| _ ->
				Globals.die "" __LOC__)
		| "type_instance_fields" ->
			(function
			| [VType t] ->
				(match t with
				| HObj o ->
					let rec fields o =
						let sup = (match o.psuper with None -> [||] | Some o -> fields o) in
						Array.concat [
							sup;
							Array.map (fun (s,_,_) -> VBytes (caml_to_hl s)) o.pfields;
							Array.of_list (Array.fold_left (fun acc f ->
								let is_override = (match o.psuper with None -> false | Some p -> try ignore(get_index f.fname p); true with Not_found -> false) in
								if is_override then acc else VBytes (caml_to_hl f.fname) :: acc
							) [] o.pproto)
						]
					in
					VArray (fields o,HBytes)
				| _ -> VNull)
			| _ -> Globals.die "" __LOC__)
		| "type_enum_fields" ->
			(function
			| [VType t] ->
				(match t with
				| HEnum e -> VArray (Array.map (fun (f,_,_) -> VBytes (caml_to_hl f)) e.efields,HBytes)
				| _ -> VNull)
			| _ -> Globals.die "" __LOC__)
		| "type_enum_values" ->
			(function
			| [VType (HEnum e)] ->
				VArray (Array.mapi (fun i (_,_,args) -> if Array.length args <> 0 then VNull else VEnum (e,i,[||])) e.efields,HDyn)
			| _ -> Globals.die "" __LOC__)
		| "type_enum_eq" ->
			(function
			| [VEnum _; VNull] | [VNull; VEnum _] -> VBool false
			| [VNull; VNull] -> VBool true
			| [VEnum (e1,_,_) as v1; VEnum (e2,_,_) as v2] ->
				let rec loop v1 v2 e =
					match v1, v2 with
					| VEnum (_,t1,_), VEnum (_,t2,_) when t1 <> t2 -> false
					| VEnum (_,t,vl1), VEnum (_,_,vl2) ->
						let _, _, pl = e.efields.(t) in
						let rec chk i =
							if i = Array.length pl then true
							else
							(match pl.(i) with
							| HEnum e -> loop vl1.(i) vl2.(i) e
							| t -> dyn_compare ctx vl1.(i) t vl2.(i) t = 0) && chk (i + 1)
						in
						chk 0
					| _ -> Globals.die "" __LOC__
				in
				VBool (if e1 != e2 then false else loop v1 v2 e1)
			| _ -> Globals.die "" __LOC__)
		| "obj_get_field" ->
			(function
			| [o;VInt hash] ->
				let f = (try Hashtbl.find ctx.cached_hashes hash with Not_found -> Globals.die "" __LOC__) in
				(match o with
				| VObj _ | VDynObj _ | VVirtual _ -> dyn_get_field ctx o f HDyn
				| _ -> VNull)
			| _ -> Globals.die "" __LOC__)
		| "obj_set_field" ->
			(function
			| [o;VInt hash;v] ->
				let f = (try Hashtbl.find ctx.cached_hashes hash with Not_found -> Globals.die "" __LOC__) in
				dyn_set_field ctx o f v HDyn;
				VUndef
			| _ -> Globals.die "" __LOC__)
		| "obj_has_field" ->
			(function
			| [o;VInt hash] ->
				let f = (try Hashtbl.find ctx.cached_hashes hash with Not_found -> Globals.die "" __LOC__) in
				let rec loop o =
					match o with
					| VDynObj d -> Hashtbl.mem d.dfields f
					| VObj o ->
						let rec loop p =
							if PMap.mem f p.pindex then let idx, _ = PMap.find f p.pindex in idx >= 0 else match p.psuper with None -> false | Some p -> loop p
						in
						loop o.oproto.pclass
					| VVirtual v -> loop v.vvalue
					| _ -> false
				in
				VBool (loop o)
			| _ -> Globals.die "" __LOC__)
		| "obj_delete_field" ->
			(function
			| [o;VInt hash] ->
				let f = (try Hashtbl.find ctx.cached_hashes hash with Not_found -> Globals.die "" __LOC__) in
				let rec loop o =
					match o with
					| VDynObj d when Hashtbl.mem d.dfields f ->
						let idx = Hashtbl.find d.dfields f in
						let count = Array.length d.dvalues in
						Hashtbl.remove d.dfields f;
						let fields = Hashtbl.fold (fun name i acc -> (name,if i < idx then i else i - 1) :: acc) d.dfields [] in
						Hashtbl.clear d.dfields;
						List.iter (fun (n,i) -> Hashtbl.add d.dfields n i) fields;
						let vals2 = Array.make (count - 1) VNull in
						let types2 = Array.make (count - 1) HVoid in
						let len = count - idx - 1 in
						Array.blit d.dvalues 0 vals2 0 idx;
						Array.blit d.dvalues (idx + 1) vals2 idx len;
						Array.blit d.dtypes 0 types2 0 idx;
						Array.blit d.dtypes (idx + 1) types2 idx len;
						d.dvalues <- vals2;
						d.dtypes <- types2;
						rebuild_virtuals ctx d;
						true
					| VVirtual v -> loop v.vvalue
					| _ -> false
				in
				VBool (loop o)
			| _ -> Globals.die "" __LOC__)
		| "get_virtual_value" ->
			(function
			| [VVirtual v] -> v.vvalue
			| _ -> Globals.die "" __LOC__)
		| "ucs2length" ->
			(function
			| [VBytes s; VInt pos] ->
				let delta = int pos in
				let rec loop p =
					let c = int_of_char s.[p+delta] lor ((int_of_char s.[p+delta+1]) lsl 8) in
					if c = 0 then p lsr 1 else loop (p + 2)
				in
				to_int (loop 0)
			| _ -> Globals.die "" __LOC__)
		| "utf8_to_utf16" ->
			(function
			| [VBytes s; VInt pos; VRef (r,HI32)] ->
				let s = String.sub s (int pos) (String.length s - (int pos)) in
				let u16 = caml_to_hl (try String.sub s 0 (String.index s '\000') with Not_found -> Globals.die "" __LOC__) in
				set_ref r (to_int (String.length u16 - 2));
				VBytes u16
			| _ -> Globals.die "" __LOC__)
		| "utf16_to_utf8" ->
			(function
			| [VBytes s; VInt pos; VRef (r,HI32)] ->
				let s = String.sub s (int pos) (String.length s - (int pos)) in
				let u8 = hl_to_caml s in
				set_ref r (to_int (String.length u8));
				VBytes (u8 ^ "\x00")
			| _ -> Globals.die "" __LOC__)
		| "ucs2_upper" ->
			(function
			| [VBytes s; VInt pos; VInt len] ->
				let buf = Buffer.create 0 in
				utf16_iter (fun c ->
					let c =
						if c >= int_of_char 'a' && c <= int_of_char 'z' then c + int_of_char 'A' - int_of_char 'a'
						else c
					in
					Common.utf16_add buf c
				) (String.sub s (int pos) ((int len) lsl 1));
				Common.utf16_add buf 0;
				VBytes (Buffer.contents buf)
			| _ -> Globals.die "" __LOC__)
		| "ucs2_lower" ->
			(function
			| [VBytes s; VInt pos; VInt len] ->
				let buf = Buffer.create 0 in
				utf16_iter (fun c ->
					let c =
						if c >= int_of_char 'A' && c <= int_of_char 'Z' then c + int_of_char 'a' - int_of_char 'A'
						else c
					in
					Common.utf16_add buf c
				) (String.sub s (int pos) ((int len) lsl 1));
				Common.utf16_add buf 0;
				VBytes (Buffer.contents buf)
			| _ -> Globals.die "" __LOC__)
		| "url_encode" ->
			(function
			| [VBytes s; VRef (r, HI32)] ->
				let s = hl_to_caml s in
				let buf = Buffer.create 0 in
				Common.url_encode s (utf16_char buf);
				Common.utf16_add buf 0;
				let str = Buffer.contents buf in
				set_ref r (to_int (String.length str lsr 1 - 1));
				VBytes str
			| _ -> Globals.die "" __LOC__)
		| "url_decode" ->
			(function
			| [VBytes s; VRef (r, HI32)] ->
				let s = hl_to_caml s in
				let b = Buffer.create 0 in
				let len = String.length s in
				let decode c =
					match c with
					| '0'..'9' -> Some (int_of_char c - int_of_char '0')
					| 'a'..'f' -> Some (int_of_char c - int_of_char 'a' + 10)
					| 'A'..'F' -> Some (int_of_char c - int_of_char 'A' + 10)
					| _ -> None
				in
				let rec loop i =
					if i = len then () else
					let c = String.unsafe_get s i in
					match c with
					| '%' ->
						let p1 = (try decode (String.get s (i + 1)) with _ -> None) in
						let p2 = (try decode (String.get s (i + 2)) with _ -> None) in
						(match p1, p2 with
						| Some c1, Some c2 ->
							Buffer.add_char b (char_of_int ((c1 lsl 4) lor c2));
							loop (i + 3)
						| _ ->
							loop (i + 1));
					| '+' ->
						Buffer.add_char b ' ';
						loop (i + 1)
					| c ->
						Buffer.add_char b c;
						loop (i + 1)
				in
				loop 0;
				let str = Buffer.contents b in
				set_ref r (to_int (UTF8.length str));
				VBytes (caml_to_hl str)
			| _ -> Globals.die "" __LOC__)
		| "call_method" ->
			(function
			| [f;VArray (args,HDyn)] -> dyn_call ctx f (List.map (fun v -> v,HDyn) (Array.to_list args)) HDyn
			| _ -> Globals.die "" __LOC__)
		| "no_closure" ->
			(function
			| [VClosure (f,_)] -> VClosure (f,None)
			| _ -> Globals.die "" __LOC__)
		| "get_closure_value" ->
			(function
			| [VClosure (_,None)] -> VNull
			| [VClosure (_,Some v)] -> v
			| _ -> Globals.die "" __LOC__)
		| "make_var_args" ->
			(function
			| [VClosure (f,arg)] -> VVarArgs (f,arg)
			| _ -> Globals.die "" __LOC__)
		| "bytes_find" ->
			(function
			| [VBytes src; VInt pos; VInt len; VBytes chk; VInt cpos; VInt clen; ] ->
				to_int (try int pos + ExtString.String.find (String.sub src (int pos) (int len)) (String.sub chk (int cpos) (int clen)) with ExtString.Invalid_string -> -1)
			| _ -> Globals.die "" __LOC__)
		| "bytes_compare" ->
			(function
			| [VBytes a; VInt apos; VBytes b; VInt bpos; VInt len] -> to_int (String.compare (String.sub a (int apos) (int len)) (String.sub b (int bpos) (int len)))
			| _ -> Globals.die "" __LOC__)
		| "string_compare" ->
			(function
			| [VBytes a; VBytes b; VInt len] -> to_int (String.compare (String.sub a 0 ((int len) * 2)) (String.sub b 0 ((int len)*2)))
			| _ -> Globals.die "" __LOC__)
		| "bytes_fill" ->
			(function
			| [VBytes a; VInt pos; VInt len; VInt v] ->
				Bytes.fill (Bytes.unsafe_of_string a) (int pos) (int len) (char_of_int ((int v) land 0xFF));
				VUndef
			| _ -> Globals.die "" __LOC__)
		| "exception_stack" ->
			(function
			| [] -> VArray (Array.map (fun e -> VBytes (caml_to_hl (stack_frame ctx e))) (Array.of_list (List.rev ctx.error_stack)),HBytes)
			| _ -> Globals.die "" __LOC__)
		| "date_new" ->
			(function
			| [VInt y; VInt mo; VInt d; VInt h; VInt m; VInt s] ->
				let t = Unix.localtime (Unix.time()) in
				let t = { t with
					tm_year = int y - 1900;
					tm_mon = int mo;
					tm_mday = int d;
					tm_hour = int h;
					tm_min = int m;
					tm_sec = int s;
				} in
				to_date t
			| _ ->
				Globals.die "" __LOC__)
		| "date_now" ->
			(function
			| [] -> to_date (Unix.localtime (Unix.time()))
			| _ -> Globals.die "" __LOC__)
		| "date_get_time" ->
			(function
			| [VInt v] -> VFloat (fst (Unix.mktime (date v)) *. 1000.)
			| _ -> Globals.die "" __LOC__)
		| "date_from_time" ->
			(function
			| [VFloat f] -> to_date (Unix.localtime (f /. 1000.))
			| _ -> Globals.die "" __LOC__)
		| "date_get_inf" ->
			(function
			| [VInt d;year;month;day;hours;minutes;seconds;wday] ->
				let d = date d in
				let set r v =
					match r with
					| VNull -> ()
					| VRef (r,HI32) -> set_ref r (to_int v)
					| _ -> Globals.die "" __LOC__
				in
				set year (d.tm_year + 1900);
				set month d.tm_mon;
				set day d.tm_mday;
				set hours d.tm_hour;
				set minutes d.tm_min;
				set seconds d.tm_sec;
				set wday d.tm_wday;
				VUndef
			| _ -> Globals.die "" __LOC__)
		| "date_to_string" ->
			(function
			| [VInt d; VRef (r,HI32)] ->
				let t = date d in
				let str = Printf.sprintf "%.4d-%.2d-%.2d %.2d:%.2d:%.2d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec in
				set_ref r (to_int (String.length str));
				VBytes (caml_to_hl str)
			| _ -> Globals.die "" __LOC__)
		| "rnd_init_system" ->
			(function
			| [] -> Random.self_init(); VAbstract ARandom
			| _ -> Globals.die "" __LOC__)
		| "rnd_int" ->
			(function
			| [VAbstract ARandom] -> VInt (Int32.of_int (Random.bits()))
			| _ -> Globals.die "" __LOC__)
		| "rnd_float" ->
			(function
			| [VAbstract ARandom] -> VFloat (Random.float 1.)
			| _ -> Globals.die "" __LOC__)
		| "regexp_new_options" ->
			(function
			| [VBytes str; VBytes opt] ->
				let case_sensitive = ref true in
				List.iter (function
					| 'm' -> () (* always ON ? *)
					| 'i' -> case_sensitive := false
					| c -> failwith ("Unsupported regexp option '" ^ String.make 1 c ^ "'")
				) (ExtString.String.explode (hl_to_caml opt));
				let buf = Buffer.create 0 in
				let rec loop prev esc = function
					| [] -> ()
					| c :: l when esc ->
						(match c with
						| 'n' -> Buffer.add_char buf '\n'
						| 'r' -> Buffer.add_char buf '\r'
						| 't' -> Buffer.add_char buf '\t'
						| 's' -> Buffer.add_string buf "[ \t\r\n]"
						| 'd' -> Buffer.add_string buf "[0-9]"
						| '\\' -> Buffer.add_string buf "\\\\"
						| '(' | ')' | '{' | '}' -> Buffer.add_char buf c
						| '1'..'9' | '+' | '$' | '^' | '*' | '?' | '.' | '[' | ']' ->
							Buffer.add_char buf '\\';
							Buffer.add_char buf c;
						| _ ->
							Buffer.add_char buf c);
						loop c false l
					| c :: l ->
						match c with
						| '\\' -> loop prev true l
						| '(' | '|' | ')' ->
							Buffer.add_char buf '\\';
							Buffer.add_char buf c;
							loop c false l
						| '?' when prev = '(' && (match l with ':' :: _ -> true | _ -> false) ->
							failwith "Non capturing groups '(?:' are not supported in macros"
						| '?' when prev = '*' ->
							failwith "Ungreedy *? are not supported in macros"
						| _ ->
							Buffer.add_char buf c;
							loop c false l
				in
				loop '\000' false (ExtString.String.explode (hl_to_caml str));
				let str = Buffer.contents buf in
				let r = {
					r = if !case_sensitive then Str.regexp str else Str.regexp_case_fold str;
					r_string = "";
					r_groups = [||];
				} in
				VAbstract (AReg r)
			| _ ->
				Globals.die "" __LOC__);
		| "regexp_match" ->
			(function
			| [VAbstract (AReg r);VBytes str;VInt pos;VInt len] ->
				let str = hl_to_caml str and pos = int pos and len = int len in
				let nstr, npos, delta = (if len = String.length str - pos then str, pos, 0 else String.sub str pos len, 0, pos) in
				(try
					ignore(Str.search_forward r.r nstr npos);
					let rec loop n =
						if n = 9 then
							[]
						else try
							(Some (Str.group_beginning n + delta, Str.group_end n + delta)) :: loop (n + 1)
						with Not_found ->
							None :: loop (n + 1)
						| Invalid_argument _ ->
							[]
					in
					r.r_string <- str;
					r.r_groups <- Array.of_list (loop 0);
					VBool true;
				with Not_found ->
					VBool false)
			| _ -> Globals.die "" __LOC__);
		| "regexp_matched_pos" ->
			(function
			| [VAbstract (AReg r); VInt n; VRef (rr,HI32)] ->
				let n = int n in
				(match (try r.r_groups.(n) with _ -> failwith ("Invalid group " ^ string_of_int n)) with
				| None -> to_int (-1)
				| Some (pos,pend) -> set_ref rr (to_int (pend - pos)); to_int pos)
			| [VAbstract (AReg r); VInt n; VNull] ->
				let n = int n in
				(match (try r.r_groups.(n) with _ -> failwith ("Invalid group " ^ string_of_int n)) with
				| None -> to_int (-1)
				| Some (pos,pend) -> to_int pos)
			| _ -> Globals.die "" __LOC__)
		| "make_macro_pos" ->
			(function
			| [VBytes file;VInt min;VInt max] ->
				VAbstract (APos { Globals.pfile = String.sub file 0 (String.length file - 1); pmin = Int32.to_int min; pmax = Int32.to_int max })
			| _ -> Globals.die "" __LOC__)
		| "dyn_op" ->
			let op_names = [|"+";"-";"*";"%";"/";"<<";">>";">>>";"&";"|";"^"|] in
			(function
			| [VInt op; a; b] ->
				let op = Int32.to_int op in
				let is_number v =
					match v with
					| VNull -> true
					| VDyn (_,t) -> is_number t
					| _ -> false
				in
				let error() =
					failwith ("Can't perform dyn op " ^ vstr ctx a HDyn ^ " " ^ op_names.(op) ^ " " ^ vstr ctx b HDyn)
				in
				let fop op =
					if is_number a && is_number b then begin
						let a = dyn_cast ctx a HDyn HF64 in
						let b = dyn_cast ctx b HDyn HF64 in
						match a, b with
						| VFloat a, VFloat b -> VDyn (VFloat (op a b),HF64)
						| _ -> Globals.die "" __LOC__
					end else
						error();
				in
				let iop op =
					if is_number a && is_number b then begin
						let a = dyn_cast ctx a HDyn HI32 in
						let b = dyn_cast ctx b HDyn HI32 in
						match a, b with
						| VInt a, VInt b -> VDyn (VInt (op a b),HI32)
						| _ -> Globals.die "" __LOC__
					end else
						error();
				in
				(match op with
				| 0 -> fop ( +. )
				| 1 -> fop ( -. )
				| 2 -> fop ( *. )
				| 3 -> fop mod_float
				| 4 -> fop ( /. )
				| 5 -> iop (fun a b -> Int32.shift_left a (Int32.to_int b))
				| 6 -> iop (fun a b -> Int32.shift_right a (Int32.to_int b))
				| 7 -> iop (fun a b -> Int32.shift_right_logical a (Int32.to_int b))
				| 8 -> iop Int32.logand
				| 9 -> iop Int32.logor
				| 10 -> iop Int32.logxor
				| _ -> Globals.die "" __LOC__)
			| _ -> Globals.die "" __LOC__)
		| _ ->
			unresolved())
	| "macro" ->
		(match ctx.resolve_macro_api name with
		| None -> unresolved()
		| Some f -> f)
	| _ ->
		unresolved()
	) in
	FNativeFun (lib ^ "@" ^ name, f, t)

let create checked =
	let ctx = {
		t_globals = [||];
		t_functions = [||];
		call_stack = [];
		error_stack = [];
		stack = [||];
		stack_pos = 0;
		cached_protos = Hashtbl.create 0;
		cached_strings = Hashtbl.create 0;
		cached_hashes = Hashtbl.create 0;
		code = {
			functions = [||];
			globals = [||];
			natives = [||];
			strings = [||];
			bytes = [||];
			ints = [||];
			debugfiles = [||];
			floats = [||];
			entrypoint = 0;
			version = 0;
			constants = [||];
		};
		checked = checked;
		fcall = (fun _ _ -> Globals.die "" __LOC__);
		on_error = (fun _ _ -> Globals.die "" __LOC__);
		resolve_macro_api = (fun _ -> None);
	} in
	ctx.on_error <- (fun msg stack -> failwith (vstr ctx msg HDyn ^ "\n" ^ String.concat "\n" (List.map (stack_frame ctx) stack)));
	ctx.fcall <- call_fun ctx;
	ctx

let set_error_handler ctx e =
	ctx.on_error <- e

let set_macro_api ctx f =
	ctx.resolve_macro_api <- f

let add_code ctx code =
	(* expand global table *)
	let globals = Array.map default code.globals in
	Array.blit ctx.t_globals 0 globals 0 (Array.length ctx.t_globals);
	ctx.t_globals <- globals;
	(* expand function table *)
	let nfunctions = Array.length code.functions + Array.length code.natives in
	let functions = Array.make nfunctions (FNativeFun ("",(fun _ -> Globals.die "" __LOC__),HDyn)) in
	Array.blit ctx.t_functions 0 functions 0 (Array.length ctx.t_functions);
	let rec loop i =
		if i = Array.length code.natives then () else
		let lib, name, t, idx = code.natives.(i) in
		functions.(idx) <- load_native ctx code.strings.(lib) code.strings.(name) t;
		loop (i + 1)
	in
	loop (Array.length ctx.code.natives);
	let rec loop i =
		if i = Array.length code.functions then () else
		let fd = code.functions.(i) in
		functions.(fd.findex) <- FFun fd;
		loop (i + 1)
	in
	loop (Array.length ctx.code.functions);
	ctx.t_functions <- functions;
	ctx.code <- code;
	Array.iter (fun (g,fields) ->
		let t = code.globals.(g) in
		let get_const_val t idx =
			match t with
			| HI32 -> VInt code.ints.(idx)
			| HBytes -> VBytes (cached_string ctx idx)
			| _ -> Globals.die "" __LOC__
		in
		let v = (match t with
		| HObj o ->
			if Array.length o.pfields <> Array.length fields then Globals.die "" __LOC__;
			let proto,_,_ = get_proto ctx o in
			VObj {
				oproto = proto;
				ofields = Array.mapi (fun i (_,_,t) -> get_const_val t fields.(i)) o.pfields;
			}
		| _ -> Globals.die "" __LOC__
		) in
		ctx.t_globals.(g) <- v;
	) code.constants;
	(* call entrypoint *)
	ignore(call_wrap ctx functions.(code.entrypoint) [])

(* ------------------------------- CHECK ---------------------------------------------- *)

let check code macros =
	let ftypes = Array.make (Array.length code.natives + Array.length code.functions) HVoid in
	let is_native_fun = Hashtbl.create 0 in

	let check_fun f =
		let pos = ref 0 in
		let error msg =
			let dfile, dline = f.debug.(!pos) in
			let file = code.debugfiles.(dfile) in
			let msg = Printf.sprintf "Check failure at fun@%d @%X - %s" f.findex (!pos) msg in
			if macros then begin
				let low = dline land 0xFFFFF in
				let pos = {
					Globals.pfile = file;
					Globals.pmin = low;
					Globals.pmax = low + (dline lsr 20);
				} in
				Common.abort msg pos
			end else
				failwith (Printf.sprintf "\n%s:%d: %s" file dline msg)
		in
		let targs, tret = (match f.ftype with HFun (args,ret) -> args, ret | _ -> Globals.die "" __LOC__) in
		let rtype i = try f.regs.(i) with _ -> HObj { null_proto with pname = "OUT_OF_BOUNDS:" ^ string_of_int i } in
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
			| HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64 -> ()
			| _ -> error (reg_inf r ^ " should be numeric")
		in
		let int r =
			match rtype r with
			| HUI8 | HUI16 | HI32 | HI64 -> ()
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
				check tret (rtype r)
			| _ -> Globals.die "" __LOC__
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
			if not (is_dynamic (rtype r)) then error (reg_inf r ^ " should be castable to dynamic")
		in
		let get_field r p fid =
			try snd (resolve_field p fid) with Not_found -> error (reg_inf r ^ " does not have field " ^ string_of_int fid)
		in
		let tfield o fid proto =
			if fid < 0 then error (reg_inf o ^ " does not have " ^ (if proto then "proto " else "") ^ "field " ^ string_of_int fid);
			match rtype o with
			| HObj p ->
				if proto then ftypes.(p.pvirtuals.(fid)) else get_field o p fid
			| HStruct p when not proto ->
				get_field o p fid
			| HVirtual v when not proto ->
				let _,_, t = v.vfields.(fid) in
				t
			| _ ->
				is_obj o;
				HVoid
		in
		list_iteri reg targs;
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
			| OBytes (r,i) ->
				reg r HBytes;
				if i < 0 || i >= Array.length code.bytes then error "bytes outside range";
			| ONull r ->
				let t = rtype r in
				if not (is_nullable t) then error (tstr t ^ " is not nullable")
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
				| HFun (tobj :: targs, tret) when List.length targs = List.length rl -> reg 0 tobj; List.iter2 reg rl targs; check tret (rtype r)
				| t -> check t (HFun (rtype 0 :: List.map rtype rl, rtype r)));
			| OCallMethod (r, m, rl) ->
				(match rl with
				| [] -> Globals.die "" __LOC__
				| obj :: rl2 ->
					let check_args targs tret rl =
						if List.length targs <> List.length rl then false else begin
							List.iter2 reg rl targs;
							check tret (rtype r);
							true;
						end
					in
					match rtype obj with
					| HVirtual v ->
						let _, _, t = v.vfields.(m) in
						(match t with
						| HMethod (args,ret) when check_args args ret rl2 -> ()
						| _ -> check t (HMethod (List.map rtype rl, rtype r)))
					| _ ->
						let t = tfield obj m true in
						match t with
						| HFun (args, ret) when check_args args ret rl -> ()
						| _ -> check t (HFun (List.map rtype rl, rtype r)))
			| OCallClosure (r,f,rl) ->
				(match rtype f with
				| HFun (targs,tret) when List.length targs = List.length rl -> List.iter2 reg rl targs; check tret (rtype r)
				| HDyn -> List.iter (fun r -> ignore(rtype r)) rl;
				| _ -> reg f (HFun(List.map rtype rl,rtype r)))
			| OGetGlobal (r,g) ->
				if not (safe_cast code.globals.(g) (rtype r)) then reg r code.globals.(g)
			| OSetGlobal (g,r) ->
				reg r code.globals.(g)
			| ORet r ->
				reg r tret
			| OJTrue (r,delta) | OJFalse (r,delta) ->
				reg r HBool;
				can_jump delta
			| OJNull (r,delta) | OJNotNull (r,delta) ->
				if not (is_nullable (rtype r)) then reg r HDyn;
				can_jump delta
			| OJUGte (a,b,delta) | OJULt (a,b,delta) | OJSGte (a,b,delta) | OJSLt (a,b,delta) | OJSGt (a,b,delta) | OJSLte (a,b,delta) | OJNotLt (a,b,delta) | OJNotGte (a,b,delta) ->
				if not (safe_cast (rtype a) (rtype b)) then reg b (rtype a);
				can_jump delta
			| OJEq (a,b,delta) | OJNotEq (a,b,delta) ->
				(match rtype a, rtype b with
				| (HObj _ | HVirtual _), (HObj _ | HVirtual _) -> ()
				| (HDyn | HFun _), (HDyn | HFun _) -> ()
				| ta, tb when safe_cast tb ta -> ()
				| _ -> reg a (rtype b));
				can_jump delta
			| OJAlways d ->
				can_jump d
			| OToDyn (r,a) ->
				(* we can still use OToDyn on nullable if we want to turn them into dynamic *)
				if is_dynamic (rtype a) then reg a HI32; (* don't wrap as dynamic types that can safely be cast to it *)
				if rtype r <> HDyn then reg r (HNull (rtype a))
			| OToSFloat (a,b) | OToUFloat (a,b) ->
				float a;
				(match rtype b with HF32 | HF64 -> () | _ -> int b);
			| OToInt (a,b) ->
				int a;
				(match rtype b with HF32 | HF64 -> () | _ -> int b);
			| OLabel _ ->
				()
			| ONew r ->
				(match rtype r with
				| HDynObj | HVirtual _ | HStruct _ -> ()
				| _ -> is_obj r)
			| OField (r,o,fid) ->
				check (tfield o fid false) (rtype r)
			| OSetField (o,fid,r) ->
				reg r (tfield o fid false)
			| OGetThis (r,fid) ->
				check (tfield 0 fid false) (rtype r)
			| OSetThis(fid,r) ->
				reg r (tfield 0 fid false)
			| OStaticClosure (r,f) ->
				reg r ftypes.(f)
			| OVirtualClosure (r,o,fid) ->
				(match rtype o with
				| HObj _ ->
					(match tfield o fid true with
					| HFun (t :: tl, tret) ->
						reg o t;
						reg r (HFun (tl,tret));
					| _ ->
						Globals.die "" __LOC__)
				| _ ->
					is_obj o)
			| OInstanceClosure (r,f,arg) ->
				(match ftypes.(f) with
				| HFun (t :: tl, tret) ->
					reg arg t;
					if not (is_nullable t) then error (reg_inf r ^ " should be nullable");
					reg r (HFun (tl,tret));
				| _ -> Globals.die "" __LOC__);
			| OThrow r ->
				reg r HDyn
			| ORethrow r ->
				reg r HDyn
			| OGetArray (v,a,i) ->
				reg a HArray;
				reg i HI32;
				ignore(rtype v);
			| OGetUI8 (r,b,p) | OGetUI16(r,b,p) ->
				reg r HI32;
				reg b HBytes;
				reg p HI32;
			| OGetMem (r,b,p) ->
				(match rtype r with HI32 | HI64 | HF32 | HF64 -> () | _ -> error (reg_inf r ^ " should be numeric"));
				reg b HBytes;
				reg p HI32;
			| OSetUI8 (r,p,v) | OSetUI16 (r,p,v) ->
				reg r HBytes;
				reg p HI32;
				reg v HI32;
			| OSetMem (r,p,v) ->
				reg r HBytes;
				reg p HI32;
				(match rtype v with HI32 | HI64 | HF32 | HF64 -> () | _ -> error (reg_inf r ^ " should be numeric"));
			| OSetArray (a,i,v) ->
				reg a HArray;
				reg i HI32;
				ignore(rtype v);
			| OUnsafeCast (a,b) ->
				is_dyn a;
				is_dyn b;
			| OSafeCast (a,b) ->
				ignore(rtype a);
				ignore(rtype b);
			| OArraySize (r,a) ->
				reg a HArray;
				reg r HI32
			| OType (r,_) ->
				reg r HType
			| OGetType (r,v) ->
				reg r HType;
				is_dyn v;
			| OGetTID (r,v) ->
				reg r HI32;
				reg v HType;
			| OUnref (v,r) ->
				(match rtype r with
				| HRef t -> check t (rtype v)
				| _ -> reg r (HRef (rtype v)))
			| ORef (r,v)
			| OSetref (r,v) ->
				(match rtype r with HRef t -> reg v t | _ -> reg r (HRef (rtype v)))
			| OToVirtual (r,v) ->
				(match rtype r with
				| HVirtual _ -> ()
				| _ -> reg r (HVirtual {vfields=[||];vindex=PMap.empty;}));
				(match rtype v with
				| HObj _ | HDynObj | HDyn | HVirtual _ -> ()
				| _ -> reg v HDynObj)
			| ODynGet (v,r,f) | ODynSet (r,f,v) ->
				ignore(code.strings.(f));
				ignore(rtype v);
				(match rtype r with
				| HObj _ | HDyn | HDynObj | HVirtual _ -> ()
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
				if rtype v <> HDyn then is_enum v;
				reg r HI32;
			| OEnumField (r,e,f,i) ->
				(match rtype e with
				| HEnum e ->
					let _, _, tl = e.efields.(f) in
					check tl.(i) (rtype r)
				| _ -> is_enum e)
			| OSetEnumField (e,i,r) ->
				(match rtype e with
				| HEnum e ->
					let _, _, tl = e.efields.(0) in
					check (rtype r) tl.(i)
				| _ -> is_enum e)
			| OSwitch (r,idx,eend) ->
				reg r HI32;
				Array.iter can_jump idx;
				if eend + 1 + i <> Array.length f.code then can_jump eend
			| ONullCheck r ->
				ignore(rtype r)
			| OTrap (r, idx) ->
				reg r HDyn;
				can_jump idx
			| OEndTrap _ ->
				()
			| OAssert _ ->
				()
			| ORefData (r,d) ->
				reg d HArray;
				(match rtype r with HRef _ -> () | _ -> reg r (HRef HDyn))
			| ORefOffset (r,r2,off) ->
				(match rtype r2 with HRef _ -> () | _ -> reg r2 (HRef HDyn));
				reg r (rtype r2);
				reg off HI32;
			| ONop _ ->
				();
		) f.code
		(* TODO : check that all path correctly initialize NULL values and reach a return *)
	in
	Array.iter (fun fd ->
		if fd.findex >= Array.length ftypes then failwith ("Invalid function index " ^ string_of_int fd.findex);
		if ftypes.(fd.findex) <> HVoid then failwith ("Duplicate function bind " ^ string_of_int fd.findex ^ " " ^ fundecl_name fd);
		ftypes.(fd.findex) <- fd.ftype;
	) code.functions;
	Array.iter (fun (lib,name,t,idx) ->
		if idx >= Array.length ftypes then failwith ("Invalid native function index " ^ string_of_int idx ^ " for "^ code.strings.(lib) ^ "@" ^ code.strings.(name));
		if ftypes.(idx) <> HVoid then failwith ("Duplicate native function bind " ^ string_of_int idx);
		Hashtbl.add is_native_fun idx true;
		ftypes.(idx) <- t
	) code.natives;
	(* TODO : check that no object type has a virtual native in his proto *)
	Array.iter check_fun code.functions

(* ------------------------------- SPEC ---------------------------------------------- *)
(*

open Hlopt

type svalue =
	| SUndef
	| SArg of int
	| SInt of int32
	| SFloat of float
	| SString of string
	| SBytes of int
	| SBool of bool
	| SNull
	| SType of ttype
	| SOp of string * svalue * svalue
	| SUnop of string * svalue
	| SResult of string
	| SFun of int * svalue option
	| SMeth of svalue * int
	| SGlobal of int
	| SField of svalue * int
	| SDField of svalue * string
	| SConv of string * svalue
	| SCast of svalue * ttype
	| SMem of svalue * svalue * ttype
	| SEnum of int * svalue list
	| SEnumField of svalue * int * int
	| SUnion of svalue list
	| SRef of int
	| SRefResult of string
	| SUnreach
	| SExc
	| SDelayed of string * svalue list option ref

type call_spec =
	| SFid of int
	| SMethod of int
	| SClosure of svalue

type spec =
	| SCall of call_spec * svalue list
	| SGlobalSet of int * svalue
	| SFieldSet of svalue * int * svalue
	| SFieldDSet of svalue * string * svalue
	| SJEq of string * svalue
	| SJComp of string * svalue * svalue
	| SJump
	| SRet of svalue
	| SNullCheck of svalue
	| SThrow of svalue
	| SSwitch of svalue
	| SWriteMem of svalue * svalue * svalue * ttype
	| SSetRef of svalue * svalue
	| SSetEnumField of svalue * int * svalue
	| SStoreResult of string * spec
	| SNew of ttype * int
	| SVal of svalue

let rec svalue_string v =
	let sval = svalue_string in
	match v with
	| SUndef -> "undef"
	| SArg i -> "arg" ^ string_of_int i
	| SInt i -> Int32.to_string i
	| SFloat f -> string_of_float f
	| SString s -> "\"" ^ s ^ "\""
	| SBytes i -> "bytes$" ^ string_of_int i
	| SBool b -> if b then "true" else "false"
	| SNull -> "null"
	| SRef _ -> "ref"
	| SRefResult s -> Printf.sprintf "refresult(%s)" s
	| SType t -> tstr t
	| SOp (op,a,b) -> Printf.sprintf "(%s %s %s)" (sval a) op (sval b)
	| SUnop (op,v) -> op ^ sval v
	| SResult i -> i
	| SFun (i,None) -> "fun" ^ string_of_int i
	| SFun (i,Some v) -> Printf.sprintf "fun%d(%s)" i (sval v)
	| SMeth (v,i) -> Printf.sprintf "meth%d(%s)" i (sval v)
	| SGlobal g -> Printf.sprintf "G[%d]" g
	| SField (o,i) -> Printf.sprintf  "%s[%d]" (sval o) i
	| SDField (o,f) -> Printf.sprintf  "%s.%s" (sval o) f
	| SConv (f,v) -> Printf.sprintf "%s(%s)" f (sval v)
	| SCast (v,t) -> Printf.sprintf "cast(%s,%s)" (sval v) (tstr t)
	| SMem (m,idx,t) -> Printf.sprintf "(%s*)%s[%s]" (tstr t) (sval m) (sval idx)
	| SEnum (i,vl) -> Printf.sprintf "enum%d(%s)" i (String.concat "," (List.map sval vl))
	| SEnumField (v,k,i) -> Printf.sprintf "%s[%d:%d]" (sval v) k i
	| SUnion vl -> Printf.sprintf "union(%s)" (String.concat " | " (List.map sval vl))
	| SUnreach -> "unreach"
	| SExc -> "exc"
	| SDelayed (str,_) -> str

let svalue_iter f = function
	| SUndef | SArg _ | SInt _ | SFloat _ | SBytes _ | SString _ | SBool _ | SNull | SType _ | SResult _
	| SFun (_,None) | SGlobal _ | SRef _ | SRefResult _ | SUnreach | SExc | SDelayed _ ->
		()
	| SOp (_,a,b) | SMem (a,b,_) -> f a; f b
	| SUnop (_,a) | SFun (_,Some a) | SMeth (a,_) | SField (a,_) | SDField (a,_) | SConv (_,a) | SCast (a,_) | SEnumField (a,_,_) -> f a
	| SUnion vl | SEnum (_,vl) -> List.iter f vl

let spec_iter fs fv = function
	| SCall (c,vl) ->
		(match c with SClosure v -> fv v | _ -> ());
		List.iter fv vl
	| SVal v
	| SJEq (_,v)
	| SRet v
	| SNullCheck v
	| SThrow v
	| SSwitch v
	| SGlobalSet (_,v) -> fv v
	| SJComp (_,a,b)
	| SSetRef (a,b)
	| SSetEnumField (a,_,b)
	| SFieldDSet (a,_,b) | SFieldSet (a,_,b) -> fv a; fv b
	| SJump ->
		()
	| SWriteMem (m,a,b,_) ->
		fv m; fv a; fv b
	| SStoreResult (_,s) ->
		fs s
	| SNew _ ->
		()

let rec svalue_same a b =
	let vsame = svalue_same in
	match a, b with
	| SType t1, SType t2 -> tsame t1 t2
	| SOp (op1,a1,b1), SOp (op2,a2,b2) -> op1 = op2 && vsame a1 a2 && vsame b1 b2
	| SUnop (op1,v1), SUnop (op2,v2) -> op1 = op2 && vsame v1 v2
	| SFun (f1,Some v1), SFun (f2,Some v2) -> f1 = f2 && vsame v1 v2
	| SMeth (v1,m1), SMeth (v2,m2) -> vsame v1 v2 && m1 = m2
	| SField (v1,f1), SField (v2,f2) -> vsame v1 v2 && f1 = f2
	| SDField (v1,f1), SDField (v2,f2) -> vsame v1 v2 && f1 = f2
	| SConv (op1,v1), SConv (op2,v2) -> op1 = op2 && vsame v1 v2
	| SCast (v1,t1), SCast (v2,t2) -> vsame v1 v2 && tsame t1 t2
	| SMem (m1,i1,t1), SMem (m2,i2,t2) -> vsame m1 m2 && vsame i1 i2 && tsame t1 t2
	| SEnum (i1,vl1), SEnum (i2,vl2) -> i1 = i2 && List.length vl1 = List.length vl2 && List.for_all2 vsame vl1 vl2
	| SEnumField (v1,c1,i1), SEnumField (v2,c2,i2) -> vsame v1 v2 && c1 = c2 && i1 = i2
	| SUnion vl1, SUnion vl2 -> List.length vl1 = List.length vl2 && List.for_all2 vsame vl1 vl2
	| SDelayed (id1,_), SDelayed (id2,_) -> id1 = id2
	| _ -> a = b

let rec spec_string s =
	let sval = svalue_string in
	match s with
	| SCall (c,vl) ->
		let cstr = (match c with
			| SFid i -> Printf.sprintf "fun%d" i
			| SMethod i -> Printf.sprintf "meth%d" i
			| SClosure v -> Printf.sprintf "closure(%s)" (sval v)
		) in
		Printf.sprintf "%s(%s)" cstr (String.concat "," (List.map sval vl))
	| SGlobalSet (i,v) ->
		Printf.sprintf "G[%d] = %s" i (sval v)
	| SFieldSet (o,fid,v) | SSetEnumField (o,fid,v) ->
		Printf.sprintf "%s[%d] = %s" (sval o) fid (sval v)
	| SFieldDSet (o,f,v) ->
		Printf.sprintf "%s.%s = %s" (sval o) f (sval v)
	| SJEq (s,v) ->
		Printf.sprintf "j%s(%s)" s (sval v)
	| SJComp (s,a,b) ->
		Printf.sprintf "jump(%s %s %s)" (sval a) s (sval b)
	| SJump ->
		"jump"
	| SRet v ->
		"ret " ^ sval v
	| SNullCheck v ->
		"nullcheck " ^ sval v
	| SThrow v ->
		"throw " ^ sval v
	| SSwitch v ->
		"switch " ^ sval v
	| SWriteMem (m,idx,v,t) ->
		Printf.sprintf "(%s*)%s[%s] = %s" (tstr t) (sval m) (sval idx) (sval v)
	| SSetRef (r,v) ->
		Printf.sprintf "*%s = %s" (sval r) (sval v)
	| SStoreResult (r,s) ->
		r ^ " <- " ^ spec_string s
	| SNew (t,idx) ->
		Printf.sprintf "new %s(%d)" (tstr t) idx
	| SVal v ->
		sval v

let make_spec (code:code) (f:fundecl) =
	let op = Array.get f.code in
	let out_spec = ref [] in
	let alloc_count = ref (-1) in

	let digest str =
		let d = Digest.to_hex (Digest.string str) in
		String.sub d 0 4
	in

	let rec semit s =
		let rec loop_spec s =
			spec_iter loop_spec loop_val s

		and loop_val v =
			match v with
			| SDelayed (r,used) ->
				(match !used with
				| None -> ()
				| Some vl -> used := None; semit (SStoreResult (r,SVal (SUnion vl))))
			| _ ->
				svalue_iter loop_val v
		in
		loop_spec s;
		out_spec := s :: !out_spec
	in

	let emit (s:spec) =
		let d = digest (spec_string s) in
		semit (SStoreResult (d,s));
		SResult d
	in

	let big_unions = Hashtbl.create 0 in

	let block_args = Hashtbl.create 0 in
	let rec get_args b =
		try
			Hashtbl.find block_args b.bstart
		with Not_found ->
			Globals.die "" __LOC__

	and calc_spec b =
		let bprev = List.filter (fun b2 -> b2.bstart < b.bstart) b.bprev in
		let args = (match bprev with
			| [] ->
				let args = Array.make (Array.length f.regs) SUndef in
				(match f.ftype with
				| HFun (tl,_) -> list_iteri (fun i _ -> args.(i) <- SArg i) tl
				| _ -> Globals.die "" __LOC__);
				args
			| b2 :: l ->
				let args = Array.copy (get_args b2) in
				List.iter (fun b2 ->
					let args2 = get_args b2 in
					for i = 0 to Array.length args - 1 do
						if not (svalue_same args.(i) args2.(i)) then begin
							let l1 = (match args.(i) with SUnion l -> l | v -> [v]) in
							let l2 = (match args2.(i) with SUnion l -> l | v -> [v]) in
							let l = l1 @ List.filter (fun v -> not (List.exists (svalue_same v) l1)) l2 in
							if List.length l > 10 then begin
								(try
									let ident, used = Hashtbl.find big_unions l in
									args.(i) <- SDelayed (ident, used);
								with Not_found ->
									let ident = digest (String.concat "," (List.map svalue_string l)) in
									let used = ref (Some l) in
									Hashtbl.replace big_unions l (ident,used);
									args.(i) <- SDelayed (ident, used))
							end else
								args.(i) <- SUnion l;
						end
					done;
				) l;
				if l = [] then (match op b2.bend with OTrap (r,_) -> args.(r) <- SExc | _ -> ());
				args
		) in
		let make_call c vl =
			let r = emit (SCall (c,vl)) in
			(match r with
			| SResult result -> List.iter (fun v -> match v with SRef r -> args.(r) <- SRefResult result | _ -> ()) vl
			| _ -> Globals.die "" __LOC__);
			r
		in
		for i = b.bstart to b.bend do
			match op i with
			| OMov (d,r) -> args.(d) <- args.(r)
			| OInt (d,i) -> args.(d) <- SInt code.ints.(i)
			| OFloat (d,f) -> args.(d) <- SFloat code.floats.(f)
			| OBool (d,b) -> args.(d) <- SBool b
			| OString (d,s) -> args.(d) <- SString code.strings.(s)
			| OBytes (d,s) -> args.(d) <- SBytes s
			| ONull d -> args.(d) <- SNull
			| OAdd (d,a,b) -> args.(d) <- SOp ("+",args.(a),args.(b))
			| OSub (d,a,b) -> args.(d) <- SOp ("-",args.(a),args.(b))
			| OMul (d,a,b) -> args.(d) <- SOp ("*",args.(a),args.(b))
			| OSDiv (d,a,b) -> args.(d) <- SOp ("/",args.(a),args.(b))
			| OUDiv (d,a,b) -> args.(d) <- SOp ("//",args.(a),args.(b))
			| OSMod (d,a,b) -> args.(d) <- SOp ("%",args.(a),args.(b))
			| OUMod (d,a,b) -> args.(d) <- SOp ("%%",args.(a),args.(b))
			| OShl (d,a,b) -> args.(d) <- SOp ("<<",args.(a),args.(b))
			| OSShr (d,a,b) -> args.(d) <- SOp (">>",args.(a),args.(b))
			| OUShr (d,a,b) -> args.(d) <- SOp (">>>",args.(a),args.(b))
			| OAnd (d,a,b) -> args.(d) <- SOp ("&",args.(a),args.(b))
			| OOr (d,a,b) -> args.(d) <- SOp ("|",args.(a),args.(b))
			| OXor (d,a,b) -> args.(d) <- SOp ("^",args.(a),args.(b))
			| ONeg (d,r) -> args.(d) <- SUnop ("-",args.(r))
			| ONot (d,r) -> args.(d) <- SUnop ("!",args.(r))
			| OIncr r -> args.(r) <- SUnop ("++",args.(r))
			| ODecr r -> args.(r) <- SUnop ("++",args.(r))
			| OCall0 (d,f) -> args.(d) <- make_call (SFid f) []
			| OCall1 (d,f,a) -> args.(d) <- make_call (SFid f) [args.(a)]
			| OCall2 (d,f,a,b) -> args.(d) <- make_call (SFid f) [args.(a);args.(b)]
			| OCall3 (d,f,a,b,c) -> args.(d) <- make_call (SFid f) [args.(a);args.(b);args.(c)]
			| OCall4 (d,f,a,b,c,k) -> args.(d) <- make_call (SFid f) [args.(a);args.(b);args.(c);args.(k)]
			| OCallN (d,f,rl) -> args.(d) <- make_call (SFid f) (List.map (fun r -> args.(r)) rl)
			| OCallMethod (d,fid,rl) -> args.(d) <- make_call (SMethod fid) (List.map (fun r -> args.(r)) rl)
			| OCallThis (d,fid,rl) -> args.(d) <- make_call (SMethod fid) (List.map (fun r -> args.(r)) (0 :: rl))
			| OCallClosure (d,r,rl) -> args.(d) <- make_call (SClosure args.(r)) (List.map (fun r -> args.(r)) rl)
			| OStaticClosure (d,fid) -> args.(d) <- SFun (fid,None)
			| OInstanceClosure (d,fid,r) -> args.(d) <- SFun (fid,Some args.(r))
			| OVirtualClosure (d,r,index) -> args.(d) <- SMeth (args.(r),index)
			| OGetGlobal (d,g) -> args.(d) <- SGlobal g
			| OSetGlobal (g,r) -> semit (SGlobalSet (g,args.(r)))
			| OField (d,r,f) -> args.(d) <- SField (args.(r),f)
			| OSetField (o,f,r) -> semit (SFieldSet (args.(o),f,args.(r)))
			| OGetThis (d,fid) -> args.(d) <- SField (args.(0),fid)
			| OSetThis (f,r) -> semit (SFieldSet (args.(0),f,args.(r)))
			| ODynGet (d,o,f) -> args.(d) <- SDField (args.(o),code.strings.(f))
			| ODynSet (o,f,v) -> semit (SFieldDSet (args.(o),code.strings.(f),args.(v)))
			| OJTrue (r,_) -> semit (SJEq ("true",args.(r)))
			| OJFalse (r,_) -> semit (SJEq ("false",args.(r)))
			| OJNull (r,_) -> semit (SJEq ("null",args.(r)))
			| OJNotNull (r,_) -> semit (SJEq ("not null",args.(r)))
			| OJSLt (a,b,_) -> semit (SJComp ("<",args.(a),args.(b)))
			| OJSGte (a,b,_) -> semit (SJComp (">=",args.(a),args.(b)))
			| OJSGt (a,b,_) -> semit (SJComp (">",args.(a),args.(b)))
			| OJSLte (a,b,_) -> semit (SJComp ("<=",args.(a),args.(b)))
			| OJULt (a,b,_) -> semit (SJComp ("<U",args.(a),args.(b)))
			| OJUGte (a,b,_) -> semit (SJComp (">=U",args.(a),args.(b)))
			| OJNotLt (a,b,_) -> semit (SJComp ("not<",args.(a),args.(b)))
			| OJNotGte (a,b,_) -> semit (SJComp ("not>=",args.(a),args.(b)))
			| OJEq (a,b,_) -> semit (SJComp ("==",args.(a),args.(b)))
			| OJNotEq (a,b,_) -> semit (SJComp ("!=",args.(a),args.(b)))
			| OJAlways _ -> semit SJump
			| OToDyn (d,r) -> args.(d) <- SConv ("dyn",args.(r))
			| OToSFloat (d,r) -> args.(d) <- SConv ("sfloat",args.(r))
			| OToUFloat (d,r) -> args.(d) <- SConv ("ufloat",args.(r))
			| OToInt (d,r) -> args.(d) <- SConv ("int",args.(r))
			| OSafeCast (d,r) -> args.(d) <- SCast (args.(r),f.regs.(d))
			| OUnsafeCast (d,r) -> args.(d) <- SConv ("cast", args.(r))
			| OToVirtual (d,r) -> args.(d) <- SConv ("virtual",args.(r))
			| OLabel _ -> ()
			| ORet r ->
				semit (SRet (if f.regs.(r) = HVoid then SUndef else args.(r)));
				if i < b.bend then for i = 0 to Array.length args - 1 do args.(i) <- SUnreach done
			| OThrow r | ORethrow r ->
				semit (SThrow args.(r));
				if i < b.bend then for i = 0 to Array.length args - 1 do args.(i) <- SUnreach done
			| OSwitch (r,_,_) -> semit (SSwitch args.(r))
			| ONullCheck r -> semit (SNullCheck args.(r))
			| OTrap _ | OEndTrap _ -> ()
			| OGetUI8 (d,b,i) -> args.(d) <- SMem (args.(b),args.(i),HUI8)
			| OGetUI16 (d,b,i) -> args.(d) <- SMem (args.(b),args.(i),HUI16)
			| OGetMem (d,b,i) -> args.(d) <- SMem (args.(b),args.(i),f.regs.(d))
			| OGetArray (d,b,i) -> args.(d) <- SMem (args.(b),args.(i),HArray)
			| OSetUI8 (b,i,v) -> semit (SWriteMem (args.(b),args.(i),args.(v),HUI8))
			| OSetUI16 (b,i,v) -> semit (SWriteMem (args.(b),args.(i),args.(v),HUI16))
			| OSetMem (b,i,v) -> semit (SWriteMem (args.(b),args.(i),args.(v),f.regs.(v)))
			| OSetArray (b,i,v) -> semit (SWriteMem (args.(b),args.(i),args.(v),HArray))
			| ONew d ->
				incr alloc_count;
				args.(d) <- emit (SNew (f.regs.(d),!alloc_count))
			| OArraySize (d,r) -> args.(d) <- SConv ("size",args.(r))
			| OType (d,t) -> args.(d) <- SType t
			| OGetType (d,r) -> args.(d) <- SConv ("type",args.(r))
			| OGetTID (d,r) -> args.(d) <- SConv ("tid",args.(r))
			| ORef (d,r) -> args.(d) <- SRef r
			| OUnref (d,r) ->
				(match args.(r) with
				| SRef r -> args.(d) <- args.(r)
				| _ -> args.(d) <- SConv ("unref",args.(r)))
			| OSetref (r,v) ->
				(match args.(r) with
				| SRef r -> args.(r) <- args.(v)
				| _ -> ());
				semit (SSetRef (args.(r),args.(v)))
			| OMakeEnum (d,fid,rl) -> args.(d) <- SEnum (fid, List.map (fun r -> args.(r)) rl)
			| OEnumAlloc (d,fid) -> args.(d) <- SEnum (fid, [])
			| OEnumIndex (d,r) -> args.(d) <- SConv ("index",args.(r))
			| OEnumField (d,r,fid,cid) -> args.(d) <- SEnumField (args.(r),fid,cid)
			| OSetEnumField (e,fid,r) -> semit (SSetEnumField (args.(e),fid,args.(r)))
			| OAssert _  -> ()
			| ONop _ -> ()
		done;
		Hashtbl.add block_args b.bstart args
	in
	let all_blocks, _ = Hlopt.code_graph f in
	let rec loop i =
		if i = Array.length f.code then () else
		if not (Hashtbl.mem all_blocks i) then loop (i + 1) else (* unreachable code *)
		let b = try Hashtbl.find all_blocks i with Not_found -> failwith (Printf.sprintf "Missing block %s(%d)" (fundecl_name f) i) in
		calc_spec b;
		loop (b.bend + 1)
	in
	loop 0;
	List.rev !out_spec
*)
