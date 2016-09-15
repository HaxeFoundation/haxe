(*
 * Copyright (C)2005-2016 Haxe Foundation
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
open Unix
open Hlcode

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
	| VVarArgs of vfunction * value option

and vabstract =
	| AHashBytes of (string, value) Hashtbl.t
	| AHashInt of (int32, value) Hashtbl.t
	| AHashObject of (value * value) list ref
	| AReg of regexp

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

and regexp = {
	r : Str.regexp;
	mutable r_string : string;
	mutable r_groups : (int * int) option array;
}

exception Return of value

let default t =
	match t with
	| HUI8 | HUI16 | HI32 -> VInt Int32.zero
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
	| VClosure (f,Some _) -> Some (match f with FFun { ftype = HFun(_::args,ret) } | FNativeFun (_,_,HFun(_::args,ret)) -> HFun (args,ret) | _ -> assert false)
	| VVarArgs _ -> Some (HFun ([],HDyn))
	| _ -> None

let v_dynamic = function
	| VNull	| VDyn _ | VObj _ | VClosure _ | VArray _ | VVirtual _ | VDynObj _ | VVarArgs _ -> true
	| _ -> false

let rec is_compatible v t =
	match v, t with
	| VInt _, (HUI8 | HUI16 | HI32) -> true
	| VFloat _, (HF32 | HF64) -> true
	| VBool _, HBool -> true
	| VNull, t -> is_nullable t
	| VObj o, HObj _ -> safe_cast (HObj o.oproto.pclass) t
	| VClosure _, HFun _ -> safe_cast (match get_type v with None -> assert false | Some t -> t) t
	| VBytes _, HBytes -> true
	| VDyn (_,t1), HNull t2 -> tsame t1 t2
	| v, HNull t -> is_compatible v t
	| v, HDyn -> v_dynamic v
	| VUndef, HVoid -> true
	| VType _, HType -> true
	| VArray _, HArray -> true
	| VDynObj _, HDynObj -> true
	| VVirtual v, HVirtual _ -> safe_cast (HVirtual v.vtype) t
	| VRef (_,_,t1), HRef t2 -> tsame t1 t2
	| VAbstract _, HAbstract _ -> true
	| VEnum _, HEnum _ -> true
	| _ -> false

exception Runtime_error of string
exception InterpThrow of value

type cast =
	| CNo
	| CDyn of ttype
	| CUnDyn of ttype
	| CCast of ttype * ttype

let interp code =

	let globals = Array.map default code.globals in
	let functions = Array.create (Array.length code.functions + Array.length code.natives) (FNativeFun ("",(fun _ -> assert false),HDyn)) in
	let cached_protos = Hashtbl.create 0 in
	let func f = Array.unsafe_get functions f in

	let stack = ref [] in
	let exc_stack = ref [] in

	let rec get_proto p =
		try
			Hashtbl.find cached_protos p.pname
		with Not_found ->
			let fields = (match p.psuper with None -> [||] | Some p -> snd(get_proto p)) in
			let meths = Array.map (fun f -> functions.(f)) p.pvirtuals in
			let fields = Array.append fields (Array.map (fun (_,_,t) -> t) p.pfields) in
			let proto = ({ pclass = p; pmethods = meths },fields) in
			Hashtbl.replace cached_protos p.pname proto;
			proto
	in

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
	in

	let utf16_eof s =
		let get v = int_of_char s.[v] in
		let rec loop p =
			let c = (get p) lor ((get (p+1)) lsl 8) in
			if c = 0 then String.sub s 0 p else loop (p + 2);
		in
		loop 0
	in

	let utf16_char buf c =
		utf16_add buf (int_of_char c)
	in

	let caml_to_hl str = utf8_to_utf16 str in

	let hl_to_caml str =
		let b = UTF8.Buf.create (String.length str / 2) in
		utf16_iter (fun c -> UTF8.Buf.add_char b (UChar.chr c)) (utf16_eof str);
		UTF8.Buf.contents b
	in

	let hl_to_caml_sub str pos len =
		hl_to_caml (String.sub str pos len ^ "\x00\x00")
	in

	let error msg = raise (Runtime_error msg) in
	let throw v = exc_stack := []; raise (InterpThrow v) in
	let throw_msg msg = throw (VDyn (VBytes (caml_to_hl msg),HBytes)) in

	let hash_cache = Hashtbl.create 0 in

	let hash str =
		let h = hl_hash str in
		if not (Hashtbl.mem hash_cache h) then Hashtbl.add hash_cache h (String.sub str 0 (try String.index str '\000' with _ -> String.length str));
		h
	in

	let null_access() =
		error "Null value bypass null pointer check"
	in

	let make_dyn v t =
		if v = VNull || is_dynamic t then
			v
		else
			VDyn (v,t)
	in

	let rec get_method p name =
		let m = ref None in
		Array.iter (fun p -> if p.fname = name then m := Some p.fmethod) p.pproto;
		match !m , p.psuper with
		| Some i, _ -> Some i
		| None, Some s -> get_method s name
		| None, None -> None
	in

	let get_to_string p =
		match get_method p "__string" with
		| Some f ->
			(match func f with
			| (FFun { ftype = HFun([_],HBytes) } as f) -> Some f
			| _ -> None)
		| None ->
			None
	in

	let invalid_comparison = 255 in

	let rec vstr_d v =
		match v with
		| VNull -> "null"
		| VInt i -> Int32.to_string i ^ "i"
		| VFloat f -> string_of_float f ^ "f"
		| VBool b -> if b then "true" else "false"
		| VDyn (v,t) -> "dyn(" ^ vstr_d v ^ ":" ^ tstr t ^ ")"
		| VObj o ->
			let p = "#" ^ o.oproto.pclass.pname in
			(match get_to_string o.oproto.pclass with
			| Some f -> p ^ ":" ^ vstr_d (fcall f [v])
			| None -> p)
		| VBytes b -> "bytes(" ^ String.escaped b ^ ")"
		| VClosure (f,o) ->
			(match o with
			| None -> fstr f
			| Some v -> fstr f ^ "[" ^ vstr_d v ^ "]")
		| VArray (a,t) -> "array<" ^ tstr t ^ ">(" ^ String.concat "," (Array.to_list (Array.map vstr_d a)) ^ ")"
		| VUndef -> "undef"
		| VType t -> "type(" ^ tstr t ^ ")"
		| VRef (regs,i,_) -> "ref(" ^ vstr_d regs.(i) ^ ")"
		| VVirtual v -> "virtual(" ^ vstr_d v.vvalue ^ ")"
		| VDynObj d -> "dynobj(" ^ String.concat "," (Hashtbl.fold (fun f i acc -> (f^":"^vstr_d d.dvalues.(i)) :: acc) d.dfields []) ^ ")"
		| VEnum (i,vals) -> "enum#" ^ string_of_int i  ^ "(" ^ String.concat "," (Array.to_list (Array.map vstr_d vals)) ^ ")"
		| VAbstract _ -> "abstract"
		| VVarArgs _ -> "varargs"

	and vstr v t =
		match v with
		| VNull -> "null"
		| VInt i -> Int32.to_string i
		| VFloat f ->
			let s = string_of_float f in
			let len = String.length s in
			if String.unsafe_get s (len - 1) = '.' then String.sub s 0 (len - 1) else s
		| VBool b -> if b then "true" else "false"
		| VDyn (v,t) ->
			vstr v t
		| VObj o ->
			(match get_to_string o.oproto.pclass with
			| None -> "#" ^ o.oproto.pclass.pname
			| Some f -> vstr (fcall f [v]) HBytes)
		| VBytes b -> (try hl_to_caml b with _ -> "?" ^ String.escaped b)
		| VClosure (f,_) -> fstr f
		| VArray (a,t) -> "[" ^ String.concat ", " (Array.to_list (Array.map (fun v -> vstr v t) a)) ^ "]"
		| VUndef -> "undef"
		| VType t -> tstr t
		| VRef (regs,i,t) -> "*" ^ (vstr regs.(i) t)
		| VVirtual v -> vstr v.vvalue HDyn
		| VDynObj d ->
			(try
				let fid = Hashtbl.find d.dfields "__string" in
				(match d.dtypes.(fid) with HFun ([],HBytes) -> () | _ -> raise Not_found);
				vstr (dyn_call d.dvalues.(fid) [] HBytes) HBytes
			with Not_found ->
				"{" ^ String.concat ", " (Hashtbl.fold (fun f i acc -> (f^":"^vstr d.dvalues.(i) d.dtypes.(i)) :: acc) d.dfields []) ^ "}")
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
		| VVarArgs _ -> "varargs"

	and fstr = function
		| FFun f -> "function@" ^ string_of_int f.findex
		| FNativeFun (s,_,_) -> "native[" ^ s ^ "]"

	and fcall f args =
		match f with
		| FFun f -> call f args
		| FNativeFun (_,f,_) ->
			try
				f args
			with InterpThrow v ->
				raise (InterpThrow v)
			| Failure msg ->
				throw_msg msg
			| e ->
				throw_msg (Printexc.to_string e)

	and rebuild_virtuals d =
		let old = d.dvirtuals in
		d.dvirtuals <- [];
		List.iter (fun v ->
			let v2 = (match to_virtual (VDynObj d) v.vtype with VVirtual v -> v | _ -> assert false) in
			v.vindexes <- v2.vindexes;
			v.vtable <- d.dvalues;
		) old;
		d.dvirtuals <- old;

	and dyn_set_field obj field v vt =
		let v, vt = (match vt with
			| HDyn ->
				(match get_type v with
				| None -> if v = VNull then VNull, HDyn else assert false
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
					rebuild_virtuals d;
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
				rebuild_virtuals d;
			)
		| VObj o ->
			(try
				let idx, t = get_index field o.oproto.pclass in
				if idx < 0 then raise Not_found;
				o.ofields.(idx) <- dyn_cast v vt t
			with Not_found ->
				throw_msg (o.oproto.pclass.pname ^ " has no field " ^ field))
		| VVirtual vp ->
			dyn_set_field vp.vvalue field v vt
		| VNull ->
			null_access()
		| _ ->
			throw_msg "Invalid object access"

	and dyn_get_field obj field rt =
		let get_with v t = dyn_cast v t rt in
		match obj with
		| VDynObj d ->
			(try
				let idx = Hashtbl.find d.dfields field in
				get_with d.dvalues.(idx) d.dtypes.(idx)
			with Not_found ->
				default rt)
		| VObj o ->
			let default rt =
				match get_method o.oproto.pclass "__get_field" with
				| None -> default rt
				| Some f ->
					get_with (fcall (func f) [obj; VInt (hash field)]) HDyn
			in
			let rec loop p =
				try
					let fid = PMap.find field p.pfunctions in
					(match functions.(fid) with
					| FFun fd as f -> get_with (VClosure (f,Some obj)) (match fd.ftype with HFun (_::args,t) -> HFun(args,t) | _ -> assert false)
					| FNativeFun _ -> assert false)
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
			dyn_get_field vp.vvalue field rt
		| VNull ->
			null_access()
		| _ ->
			throw_msg "Invalid object access"

	and dyn_cast v t rt =
		let invalid() =
			error ("Can't cast " ^ vstr_d v ^ ":"  ^ tstr t ^ " to " ^ tstr rt)
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
			(match v with VInt i -> VFloat (Int32.to_float i) | _ -> assert false)
		| (HF32|HF64), (HUI8|HUI16|HI32) ->
			(match v with VFloat f -> VInt (Int32.of_float f) | _ -> assert false)
		| (HUI8|HUI16|HI32|HF32|HF64), HNull ((HUI8|HUI16|HI32|HF32|HF64) as rt) ->
			let v = dyn_cast v t rt in
			VDyn (v,rt)
		| HBool, HNull HBool ->
			VDyn (v,HBool)
		| _, HDyn ->
			make_dyn v t
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
					| CUnDyn t -> dyn_cast v HDyn t
					| CCast (t1,t2) -> dyn_cast v t1 t2
				in
				VClosure (FNativeFun ("~convert",(fun args ->
					let args = List.map2 convert args conv in
					let ret = fcall fn (match farg with None -> args | Some a -> a :: args) in
					convert ret rconv
				),rt),None)
			| _ ->
				assert false)
		| HDyn, HFun (targs,tret) when (match v with VVarArgs _ -> true | _ -> false) ->
			VClosure (FNativeFun ("~varargs",(fun args ->
				dyn_call v (List.map2 (fun v t -> (v,t)) args targs) tret
			),rt),None)
		| HDyn, _ ->
			(match get_type v with
			| None -> assert false
			| Some t -> dyn_cast (match v with VDyn (v,_) -> v | _ -> v) t rt)
		| HNull t, _ ->
			(match v with
			| VDyn (v,t) -> dyn_cast v t rt
			| _ -> assert false)
		| HObj _, HObj b when safe_cast rt t && (match get_type v with Some t -> safe_cast t rt | None -> assert false) ->
			(* downcast *)
			v
		| (HObj _ | HDynObj | HVirtual _), HVirtual vp ->
			to_virtual v vp
		| HVirtual _, _ ->
			(match v with
			| VVirtual v -> dyn_cast v.vvalue (match get_type v.vvalue with None -> assert false | Some t -> t) rt
			| _ -> assert false)
		| HObj p, _ ->
			(match get_method p "__cast" with
			| None -> invalid()
			| Some f ->
				if v = VNull then VNull else
				let ret = fcall (func f) [v;VType rt] in
				if ret <> VNull && (match get_type ret with None -> assert false | Some vt -> safe_cast vt rt) then ret else invalid())
		| _ ->
			invalid()

	and dyn_call v args tret =
		match v with
		| VClosure (f,a) ->
			let ft = (match f with FFun f -> f.ftype | FNativeFun (_,_,t) -> t) in
			let fargs, fret = (match ft with HFun (a,t) -> a, t | _ -> assert false) in
			let full_args = args and full_fargs = (match a with None -> fargs | Some _ -> List.tl fargs) in
			let rec loop args fargs =
				match args, fargs with
				| [], [] -> []
				| _, [] -> throw_msg (Printf.sprintf "Too many arguments (%s) != (%s)" (String.concat "," (List.map (fun (v,_) -> vstr_d v) full_args)) (String.concat "," (List.map tstr full_fargs)))
				| (v,t) :: args, ft :: fargs -> dyn_cast v t ft :: loop args fargs
				| [], _ :: fargs -> default ft :: loop args fargs
			in
			let vargs = loop args full_fargs in
			let v = fcall f (match a with None -> vargs | Some a -> a :: vargs) in
			dyn_cast v fret tret
		| VNull ->
			null_access()
		| VVarArgs (f,a) ->
			let arr = VArray (Array.of_list (List.map (fun (v,t) -> make_dyn v t) args),HDyn) in
			dyn_call (VClosure (f,a)) [arr,HArray] tret
		| _ ->
			throw_msg (vstr_d v ^ " cannot be called")

	and dyn_compare a at b bt =
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
			| Some f -> (match fcall (func f) [a;b] with VInt i -> Int32.to_int i | _ -> assert false));
		| VDyn (v,t), _ ->
			dyn_compare v t b bt
		| _, VDyn (v,t) ->
			dyn_compare a at v t
		| VVirtual v, _ ->
			dyn_compare v.vvalue HDyn b bt
		| _, VVirtual v ->
			dyn_compare a at v.vvalue HDyn
		| _ ->
			invalid_comparison

	and alloc_obj t =
		match t with
		| HDynObj ->
			VDynObj { dfields = Hashtbl.create 0; dvalues = [||]; dtypes = [||]; dvirtuals = []; }
		| HObj p ->
			let p, fields = get_proto p in
			VObj { oproto = p; ofields = Array.map default fields }
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
		| _ -> assert false

	and set_i32 b p v =
		String.set b p (char_of_int ((Int32.to_int v) land 0xFF));
		String.set b (p+1) (char_of_int ((Int32.to_int (Int32.shift_right_logical v 8)) land 0xFF));
		String.set b (p+2) (char_of_int ((Int32.to_int (Int32.shift_right_logical v 16)) land 0xFF));
		String.set b (p+3) (char_of_int (Int32.to_int (Int32.shift_right_logical v 24)));

	and get_i32 b p =
		let i = int_of_char (String.get b p) in
		let j = int_of_char (String.get b (p + 1)) in
		let k = int_of_char (String.get b (p + 2)) in
		let l = int_of_char (String.get b (p + 3)) in
		Int32.logor (Int32.of_int (i lor (j lsl 8) lor (k lsl 16))) (Int32.shift_left (Int32.of_int l) 24);

	and to_virtual v vp =
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
		| VVirtual v ->
			to_virtual v.vvalue vp
		| _ ->
			error ("Invalid ToVirtual " ^ vstr_d v ^ " : " ^ tstr (HVirtual vp))

	and call f args =
		let regs = Array.create (Array.length f.regs) VUndef in
		let pos = ref 1 in

		stack := (f,pos) :: !stack;
		let fret = (match f.ftype with
			| HFun (fargs,fret) ->
				if List.length fargs <> List.length args then error (Printf.sprintf "Invalid args: (%s) should be (%s)" (String.concat "," (List.map vstr_d args)) (String.concat "," (List.map tstr fargs)));
				fret
			| _ -> assert false
		) in
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
			(*if f.findex = 0 then Printf.eprintf "%d@%d set %d = %s\n" f.findex (!pos - 1) r (vstr_d v);*)
			check v (rtype r) (fun() -> "register " ^ string_of_int r);
			Array.unsafe_set regs r v
		in
		list_iteri set args;
		let get r = Array.unsafe_get regs r in
		let global g = Array.unsafe_get globals g in
		let traps = ref [] in
		let numop iop fop a b =
			match rtype a with
			(* todo : sign-extend and mask after result for HUI8/16 *)
			| HUI8 | HUI16 | HI32 ->
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
			(* todo : sign-extend and mask after result for HUI8/16 *)
			| HUI8 | HUI16 | HI32 ->
				(match regs.(a), regs.(b) with
				| VInt a, VInt b -> VInt (f a b)
				| _ -> assert false)
			| _ ->
				assert false
		in
		let iunop iop r =
			match rtype r with
			| HUI8 | HUI16 | HI32 ->
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
		let vcompare ra rb op =
			let a = get ra in
			let b = get rb in
			let t = rtype ra in
			let r = dyn_compare a t b t in
			if r = invalid_comparison then false else op r 0
		in
		let ufloat v =
			if v < 0l then Int32.to_float v +. 4294967296. else Int32.to_float v
		in
		let rec loop() =
			let op = f.code.(!pos) in
			incr pos;
			(match op with
			| OMov (a,b) -> set a (get b)
			| OInt (r,i) -> set r (VInt code.ints.(i))
			| OFloat (r,i) -> set r (VFloat (Array.unsafe_get code.floats i))
			| OString (r,s) -> set r (VBytes (caml_to_hl code.strings.(s)))
			| OBytes (r,s) -> set r (VBytes (code.strings.(s) ^ "\x00"))
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
			| OJTrue (r,i) -> if get r = VBool true then pos := !pos + i
			| OJFalse (r,i) -> if get r = VBool false then pos := !pos + i
			| ORet r -> raise (Return regs.(r))
			| OJNull (r,i) -> if get r == VNull then pos := !pos + i
			| OJNotNull (r,i) -> if get r != VNull then pos := !pos + i
			| OJSLt (a,b,i) -> if vcompare a b (<) then pos := !pos + i
			| OJSGte (a,b,i) -> if vcompare a b (>=) then pos := !pos + i
			| OJSGt (a,b,i) -> if vcompare a b (>) then pos := !pos + i
			| OJSLte (a,b,i) -> if vcompare a b (<=) then pos := !pos + i
			| OJULt (a,b,i) -> if ucompare (get a) (get b) < 0 then pos := !pos + i
			| OJUGte (a,b,i) -> if ucompare (get a) (get b) >= 0 then pos := !pos + i
			| OJEq (a,b,i) -> if vcompare a b (=) then pos := !pos + i
			| OJNotEq (a,b,i) -> if not (vcompare a b (=)) then pos := !pos + i
			| OJAlways i -> pos := !pos + i
			| OToDyn (r,a) -> set r (make_dyn (get a) f.regs.(a))
			| OToSFloat (r,a) -> set r (match get a with VInt v -> VFloat (Int32.to_float v) | VFloat _ as v -> v | _ -> assert false)
			| OToUFloat (r,a) -> set r (match get a with VInt v -> VFloat (ufloat v) | VFloat _ as v -> v | _ -> assert false)
			| OToInt (r,a) -> set r (match get a with VFloat v -> VInt (Int32.of_float v) | VInt _ as v -> v | _ -> assert false)
			| OLabel _ -> ()
			| ONew r ->
				set r (alloc_obj (rtype r))
			| OField (r,o,fid) ->
				set r (match get o with
					| VObj v -> v.ofields.(fid)
					| VVirtual v as obj ->
						(match v.vindexes.(fid) with
						| VFNone -> dyn_get_field obj (let n,_,_ = v.vtype.vfields.(fid) in n) (rtype r)
						| VFIndex i -> v.vtable.(i))
					| VNull -> null_access()
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
					| VFNone ->
						dyn_set_field o (let n,_,_ = v.vtype.vfields.(fid) in n) rv (rtype r)
					| VFIndex i ->
						check_obj rv o fid;
						v.vtable.(i) <- rv)
				| VNull -> null_access()
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
				| VVirtual v ->
					let name, _, _ = v.vtype.vfields.(m) in
					(match v.vvalue with
					| VObj o as obj ->
						(try
							let m = PMap.find name o.oproto.pclass.pfunctions in
							set r (dyn_call (VClosure (functions.(m),Some obj)) (List.map (fun r -> get r, rtype r) (List.tl rl)) (rtype r))
						with Not_found ->
							assert false)
					| VDynObj _ ->
						set r (dyn_call v.vvalue (List.map (fun r -> get r, rtype r) (List.tl rl)) (rtype r))
					| _ ->
						assert false)
				| VNull -> null_access()
				| _ -> assert false)
			| OCallThis (r,m,rl) ->
				(match get 0 with
				| VObj v as o -> set r (fcall v.oproto.pmethods.(m) (o :: List.map get rl))
				| _ -> assert false)
			| OCallClosure (r,v,rl) ->
				if rtype v = HDyn then
					set r (dyn_call (get v) (List.map (fun r -> get r, rtype r) rl) (rtype r))
				else (match get v with
				| VClosure (f,None) -> set r (fcall f (List.map get rl))
				| VClosure (f,Some arg) -> set r (fcall f (arg :: List.map get rl))
				| VNull -> null_access()
				| _ -> assert false)
			| OStaticClosure (r, fid) ->
				let f = functions.(fid) in
				set r (VClosure (f,None))
			| OInstanceClosure (r, fid, v) ->
				let f = functions.(fid) in
				set r (VClosure (f,Some (get v)))
			| OVirtualClosure (r, o, m) ->
				let m = (match get o with
				| VObj v as obj -> VClosure (v.oproto.pmethods.(m), Some obj)
				| VNull -> null_access()
				| VVirtual v ->
					let name, _, _ = v.vtype.vfields.(m) in
					(match v.vvalue with
					| VObj o as obj ->
						(try
							let m = PMap.find name o.oproto.pclass.pfunctions in
							VClosure (functions.(m), Some obj)
						with Not_found ->
							VNull)
					| _ -> assert false)
				| _ -> assert false
				) in
				set r (if m = VNull then m else dyn_cast m (match get_type m with None -> assert false | Some v -> v) (rtype r))
			| OThrow r ->
				throw (get r)
			| ORethrow r ->
				stack := List.rev !exc_stack @ !stack;
				throw (get r)
			| OGetI8 (r,b,p) ->
				(match get b, get p with
				| VBytes b, VInt p -> set r (VInt (Int32.of_int (int_of_char (String.get b (Int32.to_int p)))))
				| _ -> assert false)
			| OGetI16 (r,b,p) ->
				(match get b, get p with
				| VBytes b, VInt p ->
					let a = int_of_char (String.get b (Int32.to_int p)) in
					let b = int_of_char (String.get b (Int32.to_int p + 1)) in
					set r (VInt (Int32.of_int (a lor (b lsl 8))))
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
					let i64 = Int64.logor (Int64.logand (Int64.of_int32 (get_i32 b p)) 0xFFFFFFFFL) (Int64.shift_left (Int64.of_int32 (get_i32 b (p + 4))) 32) in
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
			| OSetI16 (r,p,v) ->
				(match get r, get p, get v with
				| VBytes b, VInt p, VInt v ->
					String.set b (Int32.to_int p) (char_of_int ((Int32.to_int v) land 0xFF));
					String.set b (Int32.to_int p + 1) (char_of_int (((Int32.to_int v) lsr 8) land 0xFF))
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
				set r (dyn_cast (get v) (rtype v) (rtype r))
			| OUnsafeCast (r,v) ->
				set r (get v)
			| OArraySize (r,a) ->
				(match get a with
				| VArray (a,_) -> set r (VInt (Int32.of_int (Array.length a)));
				| _ -> assert false)
			| OType (r,t) ->
				set r (VType t)
			| OGetType (r,v) ->
				let v = get v in
				let v = (match v with VVirtual v -> v.vvalue | _ -> v) in
				set r (VType (if v = VNull then HVoid else match get_type v with None -> assert false | Some t -> t));
			| OGetTID (r,v) ->
				set r (match get v with
					| VType t ->
						(VInt (Int32.of_int (match t with
						| HVoid -> 0
						| HUI8 -> 1
						| HUI16 -> 2
						| HI32 -> 3
						| HF32 -> 4
						| HF64 -> 5
						| HBool -> 6
						| HBytes -> 7
						| HDyn -> 8
						| HFun _ -> 9
						| HObj _ -> 10
						| HArray -> 11
						| HType -> 12
						| HRef _ -> 13
						| HVirtual _ -> 14
						| HDynObj -> 15
						| HAbstract _ -> 16
						| HEnum _ -> 17
						| HNull _ -> 18)))
					| _ -> assert false);
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
				set r (to_virtual (get rv) (match rtype r with HVirtual vp -> vp | _ -> assert false))
			| ODynGet (r,o,f) ->
				set r (dyn_get_field (get o) code.strings.(f) (rtype r))
			| ODynSet (o,fid,vr) ->
				dyn_set_field (get o) code.strings.(fid) (get vr) (rtype vr)
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
				| VEnum (i,_) | VDyn (VEnum (i,_),_) -> set r (VInt (Int32.of_int i))
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
			| OSwitch (r, indexes, _) ->
				(match get r with
				| VInt i ->
					let i = Int32.to_int i in
					if i >= 0 && i < Array.length indexes then pos := !pos + indexes.(i)
				| _ -> assert false)
			| ONullCheck r ->
				if get r = VNull then throw_msg "Null access"
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
				| Return v ->
					check v fret (fun() -> "return value");
					stack := List.tl !stack;
					v
				| InterpThrow v ->
					match !traps with
					| [] ->
						exc_stack := List.hd !stack :: !exc_stack;
						stack := List.tl !stack;
						raise (InterpThrow v)
					| (r,target) :: tl ->
						traps := tl;
						exc_stack := (f,ref !pos) :: !exc_stack;
						pos := target;
						set r v;
						exec()
		in
		pos := 0;
		exec()
	in
	let int = Int32.to_int in
	let to_int i = VInt (Int32.of_int i) in
	let date d =
		Unix.localtime (Int32.to_float d)
	in
	let to_date d =
		let t, _ = Unix.mktime d in
		VInt (Int32.of_float t)
	in
	let no_virtual v =
		match v with
		| VVirtual v -> v.vvalue
		| _ -> v
	in
	let load_native lib name t =
		let unresolved() = (fun args -> error ("Unresolved native " ^ lib ^ "@" ^ name)) in
		let f = (match lib with
		| "std" ->
			(match name with
			| "alloc_bytes" ->
				(function
				| [VInt i] -> VBytes (String.create (int i))
				| _ -> assert false)
			| "alloc_array" ->
				(function
				| [VType t;VInt i] -> VArray (Array.create (int i) (default t),t)
				| _ -> assert false)
			| "alloc_obj" ->
				(function
				| [VType t] -> alloc_obj t
				| _ -> assert false)
			| "alloc_enum" ->
				(function
				| [VType (HEnum e); VInt idx; VArray (vl,vt)] ->
					let idx = int idx in
					let _, _, args = e.efields.(idx) in
					if Array.length args <> Array.length vl then
						VNull
					else
						VDyn (VEnum (idx,Array.mapi (fun i v -> dyn_cast v vt args.(i)) vl),HEnum e)
				| _ -> assert false)
			| "array_blit" ->
				(function
				| [VArray (dst,_); VInt dp; VArray (src,_); VInt sp; VInt len] ->
					Array.blit src (int sp) dst (int dp) (int len);
					VUndef
				| _ -> assert false)
			| "bytes_blit" ->
				(function
				| [VBytes dst; VInt dp; VBytes src; VInt sp; VInt len] ->
					String.blit src (int sp) dst (int dp) (int len);
					VUndef
				| _ -> assert false)
			| "bsort_i32" ->
				(function
				| [VBytes b; VInt pos; VInt len; VClosure (f,c)] ->
					let pos = int pos and len = int len in
					let a = Array.init len (fun i -> get_i32 b (pos + i * 4)) in
					Array.stable_sort (fun a b ->
						match fcall f (match c with None -> [VInt a;VInt b] | Some v -> [v;VInt a;VInt b]) with
						| VInt i -> int i
						| _ -> assert false
					) a;
					Array.iteri (fun i v -> set_i32 b (pos + i * 4) v) a;
					VUndef;
				| _ ->
					assert false)
			| "bsort_f64" ->
				(function
				| [VBytes b; VInt pos; VInt len; VClosure _] ->
					assert false
				| _ ->
					assert false)
			| "itos" ->
				(function
				| [VInt v; VRef (regs,i,_)] ->
					let str = Int32.to_string v in
					regs.(i) <- to_int (String.length str);
					VBytes (caml_to_hl str)
				| _ -> assert false);
			| "ftos" ->
				(function
				| [VFloat _ as v; VRef (regs,i,_)] ->
					let str = vstr v HF64 in
					regs.(i) <- to_int (String.length str);
					VBytes (caml_to_hl str)
				| _ -> assert false);
			| "value_to_string" ->
				(function
				| [v; VRef (regs,i,_)] ->
					let str = caml_to_hl (vstr v HDyn) in
					regs.(i) <- to_int ((String.length str) lsr 1 - 1);
					VBytes str
				| _ -> assert false);
			| "math_isnan" -> (function [VFloat f] -> VBool (classify_float f = FP_nan) | _ -> assert false)
			| "math_isfinite" -> (function [VFloat f] -> VBool (match classify_float f with FP_infinite | FP_nan -> false | _ -> true) | _ -> assert false)
			| "math_round" -> (function [VFloat f] -> VInt (Int32.of_float (floor (f +. 0.5))) | _ -> assert false)
			| "math_floor" -> (function [VFloat f] -> VInt (Int32.of_float (floor f)) | _ -> assert false)
			| "math_ceil" -> (function [VFloat f] -> VInt (Int32.of_float (ceil f)) | _ -> assert false)
			| "math_ffloor" -> (function [VFloat f] -> VFloat (floor f) | _ -> assert false)
			| "math_fceil" -> (function [VFloat f] -> VFloat (ceil f) | _ -> assert false)
			| "math_fround" -> (function [VFloat f] -> VFloat (floor (f +. 0.5)) | _ -> assert false)
			| "math_abs" -> (function [VFloat f] -> VFloat (abs_float f) | _ -> assert false)
			| "math_sqrt" -> (function [VFloat f] -> VFloat (sqrt f) | _ -> assert false)
			| "math_cos" -> (function [VFloat f] -> VFloat (cos f) | _ -> assert false)
			| "math_sin" -> (function [VFloat f] -> VFloat (sin f) | _ -> assert false)
			| "math_tan" -> (function [VFloat f] -> VFloat (tan f) | _ -> assert false)
			| "math_acos" -> (function [VFloat f] -> VFloat (acos f) | _ -> assert false)
			| "math_asin" -> (function [VFloat f] -> VFloat (asin f) | _ -> assert false)
			| "math_atan" -> (function [VFloat f] -> VFloat (atan f) | _ -> assert false)
			| "math_atan2" -> (function [VFloat a; VFloat b] -> VFloat (atan2 a b) | _ -> assert false)
			| "math_log" -> (function [VFloat f] -> VFloat (Pervasives.log f) | _ -> assert false)
			| "math_exp" -> (function [VFloat f] -> VFloat (exp f) | _ -> assert false)
			| "math_pow" -> (function [VFloat a; VFloat b] -> VFloat (a ** b) | _ -> assert false)
			| "parse_int" ->
				(function
				| [VBytes str; VInt pos; VInt len] ->
					(try
						let i = (match Interp.parse_int (hl_to_caml_sub str (int pos) (int len)) with
							| Interp.VInt v -> Int32.of_int v
							| Interp.VInt32 v -> v
							| _ -> assert false
						) in
						VDyn (VInt i,HI32)
					with _ ->
						VNull)
				| l -> assert false)
			| "parse_float" ->
				(function
				| [VBytes str; VInt pos; VInt len] -> (try VFloat (Interp.parse_float (hl_to_caml_sub str (int pos) (int len))) with _ -> VFloat nan)
				| _ -> assert false)
			| "dyn_compare" ->
				(function
				| [a;b] -> to_int (dyn_compare a HDyn b HDyn)
				| _ -> assert false)
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
				| _ -> assert false)
			| "value_cast" ->
				(function
				| [v;VType t] -> if is_compatible v t then v else throw_msg ("Cannot cast " ^ vstr_d v ^ " to " ^ tstr t);
				| _ -> assert false)
			| "hballoc" ->
				(function
				| [] -> VAbstract (AHashBytes (Hashtbl.create 0))
				| _ -> assert false)
			| "hbset" ->
				(function
				| [VAbstract (AHashBytes h);VBytes b;v] ->
					Hashtbl.replace h (hl_to_caml b) v;
					VUndef
				| _ -> assert false)
			| "hbget" ->
				(function
				| [VAbstract (AHashBytes h);VBytes b] ->
					(try Hashtbl.find h (hl_to_caml b) with Not_found -> VNull)
				| _ -> assert false)
			| "hbvalues" ->
				(function
				| [VAbstract (AHashBytes h)] ->
					let values = Hashtbl.fold (fun _ v acc -> v :: acc) h [] in
					VArray (Array.of_list values, HDyn)
				| _ -> assert false)
			| "hbkeys" ->
				(function
				| [VAbstract (AHashBytes h)] ->
					let keys = Hashtbl.fold (fun s _ acc -> VBytes (caml_to_hl s) :: acc) h [] in
					VArray (Array.of_list keys, HBytes)
				| _ -> assert false)
			| "hbexists" ->
				(function
				| [VAbstract (AHashBytes h);VBytes b] -> VBool (Hashtbl.mem h (hl_to_caml b))
				| _ -> assert false)
			| "hbremove" ->
				(function
				| [VAbstract (AHashBytes h);VBytes b] ->
					let m = Hashtbl.mem h (hl_to_caml b) in
					if m then Hashtbl.remove h (hl_to_caml b);
					VBool m
				| _ -> assert false)
			| "hialloc" ->
				(function
				| [] -> VAbstract (AHashInt (Hashtbl.create 0))
				| _ -> assert false)
			| "hiset" ->
				(function
				| [VAbstract (AHashInt h);VInt i;v] ->
					Hashtbl.replace h i v;
					VUndef
				| _ -> assert false)
			| "higet" ->
				(function
				| [VAbstract (AHashInt h);VInt i] ->
					(try Hashtbl.find h i with Not_found -> VNull)
				| _ -> assert false)
			| "hivalues" ->
				(function
				| [VAbstract (AHashInt h)] ->
					let values = Hashtbl.fold (fun _ v acc -> v :: acc) h [] in
					VArray (Array.of_list values, HDyn)
				| _ -> assert false)
			| "hikeys" ->
				(function
				| [VAbstract (AHashInt h)] ->
					let keys = Hashtbl.fold (fun i _ acc -> VInt i :: acc) h [] in
					VArray (Array.of_list keys, HI32)
				| _ -> assert false)
			| "hiexists" ->
				(function
				| [VAbstract (AHashInt h);VInt i] -> VBool (Hashtbl.mem h i)
				| _ -> assert false)
			| "hiremove" ->
				(function
				| [VAbstract (AHashInt h);VInt i] ->
					let m = Hashtbl.mem h i in
					if m then Hashtbl.remove h i;
					VBool m
				| _ -> assert false)
			| "hoalloc" ->
				(function
				| [] -> VAbstract (AHashObject (ref []))
				| _ -> assert false)
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
				| _ -> assert false)
			| "hoget" ->
				(function
				| [VAbstract (AHashObject l);o] ->
					(try List.assq (no_virtual o) !l with Not_found -> VNull)
				| _ -> assert false)
			| "hovalues" ->
				(function
				| [VAbstract (AHashObject l)] ->
					VArray (Array.of_list (List.map snd !l), HDyn)
				| _ -> assert false)
			| "hokeys" ->
				(function
				| [VAbstract (AHashObject l)] ->
					VArray (Array.of_list (List.map fst !l), HDyn)
				| _ -> assert false)
			| "hoexists" ->
				(function
				| [VAbstract (AHashObject l);o] -> VBool (List.mem_assq (no_virtual o) !l)
				| _ -> assert false)
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
				| _ -> assert false)
			| "sys_print" ->
				(function
				| [VBytes str] -> print_string (hl_to_caml str); VUndef
				| _ -> assert false)
			| "sys_time" ->
				(function
				| [] -> VFloat (Unix.time())
				| _ -> assert false)
			| "sys_exit" ->
				(function
				| [VInt code] -> exit (Int32.to_int code)
				| _ -> assert false)
			| "sys_utf8_path" ->
				(function
				| [] -> VBool true
				| _ -> assert false)
			| "hash" ->
				(function
				| [VBytes str] -> VInt (hash (hl_to_caml str))
				| _ -> assert false)
			| "type_safe_cast" ->
				(function
				| [VType a; VType b] -> VBool (safe_cast a b)
				| _ -> assert false)
			| "type_super" ->
				(function
				| [VType t] -> VType (match t with HObj { psuper = Some o } -> HObj o | _ -> HVoid)
				| _ -> assert false)
			| "type_args_count" ->
				(function
				| [VType t] -> to_int (match t with HFun (args,_) -> List.length args | _ -> 0)
				| _ -> assert false)
			| "type_get_global" ->
				(function
				| [VType t] ->
					(match t with
					| HObj c -> (match c.pclassglobal with None -> VNull | Some g -> globals.(g))
					| HEnum e -> (match e.eglobal with None -> VNull | Some g -> globals.(g))
					| _ -> VNull)
				| _ -> assert false)
			| "type_name" ->
				(function
				| [VType t] ->
					VBytes (caml_to_hl (match t with
					| HObj o -> o.pname
					| HEnum e -> e.ename
					| _ -> assert false))
				| _ -> assert false)
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
				| _ -> assert false)
			| "obj_copy" ->
				(function
				| [VDynObj d | VVirtual { vvalue = VDynObj d }] ->
					VDynObj { dfields = Hashtbl.copy d.dfields; dvalues = Array.copy d.dvalues; dtypes = Array.copy d.dtypes; dvirtuals = [] }
				| [_] -> VNull
				| _ -> assert false)
			| "enum_parameters" ->
				(function
				| [VDyn (VEnum (idx,pl),HEnum e)] ->
					let _,_, ptypes = e.efields.(idx) in
					VArray (Array.mapi (fun i v -> make_dyn v ptypes.(i)) pl,HDyn)
				| _ ->
					assert false)
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
				| _ -> assert false)
			| "type_enum_fields" ->
				(function
				| [VType t] ->
					(match t with
					| HEnum e -> VArray (Array.map (fun (f,_,_) -> VBytes (caml_to_hl f)) e.efields,HBytes)
					| _ -> VNull)
				| _ -> assert false)
			| "type_enum_eq" ->
				(function
				| [VDyn (VEnum _, HEnum _); VNull] | [VNull; VDyn (VEnum _, HEnum _)] -> VBool false
				| [VNull; VNull] -> VBool true
				| [VDyn (VEnum _ as v1, HEnum e1); VDyn (VEnum _ as v2, HEnum e2)] ->
					let rec loop v1 v2 e =
						match v1, v2 with
						| VEnum (t1,_), VEnum (t2,_) when t1 <> t2 -> false
						| VEnum (t,vl1), VEnum (_,vl2) ->
							let _, _, pl = e.efields.(t) in
							let rec chk i =
								if i = Array.length pl then true
								else
								(match pl.(i) with
								| HEnum e -> loop vl1.(i) vl2.(i) e
								| t -> dyn_compare vl1.(i) t vl2.(i) t = 0) && chk (i + 1)
							in
							chk 0
						| _ -> assert false
					in
					VBool (if e1 != e2 then false else loop v1 v2 e1)
				| _ -> assert false)
			| "obj_get_field" ->
				(function
				| [o;VInt hash] ->
					let f = (try Hashtbl.find hash_cache hash with Not_found -> assert false) in
					(match o with
					| VObj _ | VDynObj _ | VVirtual _ -> dyn_get_field o f HDyn
					| _ -> VNull)
				| _ -> assert false)
			| "obj_set_field" ->
				(function
				| [o;VInt hash;v] ->
					let f = (try Hashtbl.find hash_cache hash with Not_found -> assert false) in
					dyn_set_field o f v HDyn;
					VUndef
				| _ -> assert false)
			| "obj_has_field" ->
				(function
				| [o;VInt hash] ->
					let f = (try Hashtbl.find hash_cache hash with Not_found -> assert false) in
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
				| _ -> assert false)
			| "obj_delete_field" ->
				(function
				| [o;VInt hash] ->
					let f = (try Hashtbl.find hash_cache hash with Not_found -> assert false) in
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
							rebuild_virtuals d;
							true
						| VVirtual v -> loop v.vvalue
						| _ -> false
					in
					VBool (loop o)
				| _ -> assert false)
			| "get_virtual_value" ->
				(function
				| [VVirtual v] -> v.vvalue
				| _ -> assert false)
			| "ucs2length" ->
				(function
				| [VBytes s; VInt pos] ->
					let delta = int pos in
					let rec loop p =
						let c = int_of_char s.[p+delta] lor ((int_of_char s.[p+delta+1]) lsl 8) in
						if c = 0 then p lsr 1 else loop (p + 2)
					in
					to_int (loop 0)
				| _ -> assert false)
			| "utf8_to_utf16" ->
				(function
				| [VBytes s; VInt pos; VRef (regs,idx,HI32)] ->
					let s = String.sub s (int pos) (String.length s - (int pos)) in
					let u16 = caml_to_hl (try String.sub s 0 (String.index s '\000') with Not_found -> assert false) in
					regs.(idx) <- to_int (String.length u16 - 2);
					VBytes u16
				| _ -> assert false)
			| "utf16_to_utf8" ->
				(function
				| [VBytes s; VInt pos; VRef (regs,idx,HI32)] ->
					let s = String.sub s (int pos) (String.length s - (int pos)) in
					let u8 = hl_to_caml s in
					regs.(idx) <- to_int (String.length u8);
					VBytes (u8 ^ "\x00")
				| _ -> assert false)
			| "ucs2_upper" ->
				(function
				| [VBytes s; VInt pos; VInt len] ->
					let buf = Buffer.create 0 in
					utf16_iter (fun c ->
						let c =
							if c >= int_of_char 'a' && c <= int_of_char 'z' then c + int_of_char 'A' - int_of_char 'a'
							else c
						in
						utf16_add buf c
					) (String.sub s (int pos) ((int len) lsl 1));
					utf16_add buf 0;
					VBytes (Buffer.contents buf)
				| _ -> assert false)
			| "ucs2_lower" ->
				(function
				| [VBytes s; VInt pos; VInt len] ->
					let buf = Buffer.create 0 in
					utf16_iter (fun c ->
						let c =
							if c >= int_of_char 'A' && c <= int_of_char 'Z' then c + int_of_char 'a' - int_of_char 'A'
							else c
						in
						utf16_add buf c
					) (String.sub s (int pos) ((int len) lsl 1));
					utf16_add buf 0;
					VBytes (Buffer.contents buf)
				| _ -> assert false)
			| "url_encode" ->
				(function
				| [VBytes s; VRef (regs, idx, HI32)] ->
					let s = hl_to_caml s in
					let buf = Buffer.create 0 in
					let hex = "0123456789ABCDEF" in
					for i = 0 to String.length s - 1 do
						let c = String.unsafe_get s i in
						match c with
						| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' | '.' ->
							utf16_char buf c
						| _ ->
							utf16_char buf '%';
							utf16_char buf (String.unsafe_get hex (int_of_char c lsr 4));
							utf16_char buf (String.unsafe_get hex (int_of_char c land 0xF));
					done;
					utf16_add buf 0;
					let str = Buffer.contents buf in
					regs.(idx) <- to_int (String.length str lsr 1 - 1);
					VBytes str
				| _ -> assert false)
			| "url_decode" ->
				(function
				| [VBytes s; VRef (regs, idx, HI32)] ->
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
					regs.(idx) <- to_int (UTF8.length str);
					VBytes (caml_to_hl str)
				| _ -> assert false)
			| "call_method" ->
				(function
				| [f;VArray (args,HDyn)] -> dyn_call f (List.map (fun v -> v,HDyn) (Array.to_list args)) HDyn
				| _ -> assert false)
			| "no_closure" ->
				(function
				| [VClosure (f,_)] -> VClosure (f,None)
				| _ -> assert false)
			| "get_closure_value" ->
				(function
				| [VClosure (_,None)] -> VNull
				| [VClosure (_,Some v)] -> v
				| _ -> assert false)
			| "make_var_args" ->
				(function
				| [VClosure (f,arg)] -> VVarArgs (f,arg)
				| _ -> assert false)
			| "bytes_find" ->
				(function
				| [VBytes src; VInt pos; VInt len; VBytes chk; VInt cpos; VInt clen; ] ->
					to_int (try int pos + ExtString.String.find (String.sub src (int pos) (int len)) (String.sub chk (int cpos) (int clen)) with ExtString.Invalid_string -> -1)
				| _ -> assert false)
			| "bytes_compare" ->
				(function
				| [VBytes a; VInt apos; VBytes b; VInt bpos; VInt len] -> to_int (String.compare (String.sub a (int apos) (int len)) (String.sub b (int bpos) (int len)))
				| _ -> assert false)
			| "bytes_fill" ->
				(function
				| [VBytes a; VInt pos; VInt len; VInt v] ->
					String.fill a (int pos) (int len) (char_of_int ((int v) land 0xFF));
					VUndef
				| _ -> assert false)
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
					assert false)
			| "date_now" ->
				(function
				| [] -> to_date (Unix.localtime (Unix.time()))
				| _ -> assert false)
			| "date_get_time" ->
				(function
				| [VInt v] -> VFloat (fst (Unix.mktime (date v)) *. 1000.)
				| _ -> assert false)
			| "date_from_time" ->
				(function
				| [VFloat f] -> to_date (Unix.localtime (f /. 1000.))
				| _ -> assert false)
			| "date_get_inf" ->
				(function
				| [VInt d;year;month;day;hours;minutes;seconds;wday] ->
					let d = date d in
					let set r v =
						match r with
						| VNull -> ()
						| VRef (regs,pos,HI32) -> regs.(pos) <- to_int v
						| _ -> assert false
					in
					set year (d.tm_year + 1900);
					set month d.tm_mon;
					set day d.tm_mday;
					set hours d.tm_hour;
					set minutes d.tm_min;
					set seconds d.tm_sec;
					set wday d.tm_wday;
					VUndef
				| _ -> assert false)
			| "date_to_string" ->
				(function
				| [VInt d; VRef (regs,pos,HI32)] ->
					let t = date d in
					let str = Printf.sprintf "%.4d-%.2d-%.2d %.2d:%.2d:%.2d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec in
					regs.(pos) <- to_int (String.length str);
					VBytes (caml_to_hl str)
				| _ -> assert false)
			| "random" ->
				(function
				| [VInt max] -> VInt (if max <= 0l then 0l else Random.int32 max)
				| _ -> assert false)
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
							| _ -> failwith ("Unsupported escaped char '" ^ String.make 1 c ^ "'"));
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
					assert false);
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
				| _ -> assert false);
			| "regexp_matched_pos" ->
				(function
				| [VAbstract (AReg r); VInt n; VRef (regs,rlen,HI32)] ->
					let n = int n in
					(match (try r.r_groups.(n) with _ -> failwith ("Invalid group " ^ string_of_int n)) with
					| None -> to_int (-1)
					| Some (pos,pend) -> regs.(rlen) <- to_int (pend - pos); to_int pos)
				| _ -> assert false)
			| _ ->
				unresolved())
		| _ ->
			unresolved()
		) in
		FNativeFun (lib ^ "@" ^ name, f, t)
	in
	Array.iter (fun (lib,name,t,idx) -> functions.(idx) <- load_native code.strings.(lib) code.strings.(name) t) code.natives;
	Array.iter (fun fd -> functions.(fd.findex) <- FFun fd) code.functions;
	let get_stack st =
		String.concat "\n" (List.map (fun (f,pos) ->
			let pos = !pos - 1 in
			let file, line = (try let fid, line = f.debug.(pos) in code.debugfiles.(fid), line with _ -> "???", 0) in
			Printf.sprintf "%s:%d: Called from fun(%d)@%d" file line f.findex pos
		) st)
	in
	match functions.(code.entrypoint) with
	| FFun f when f.ftype = HFun([],HVoid) ->
		(try
			ignore(call f [])
		with
			| InterpThrow v -> failwith ("Uncaught exception " ^ vstr v HDyn ^ "\n" ^ get_stack (List.rev !exc_stack))
			| Runtime_error msg -> failwith ("HL Interp error " ^ msg ^ "\n" ^ get_stack !stack)
		)
	| _ -> assert false

(* ------------------------------- CHECK ---------------------------------------------- *)

let check code =
	let ftypes = Array.create (Array.length code.natives + Array.length code.functions) HVoid in
	let is_native_fun = Hashtbl.create 0 in

	let check_fun f =
		let pos = ref 0 in
		let error msg =
			let dfile, dline = f.debug.(!pos) in
			failwith (Printf.sprintf "\n%s:%d: Check failure at %d@%d - %s" code.debugfiles.(dfile) dline f.findex (!pos) msg)
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
			| HUI8 | HUI16 | HI32 | HF32 | HF64 -> ()
			| _ -> error (reg_inf r ^ " should be numeric")
		in
		let int r =
			match rtype r with
			| HUI8 | HUI16 | HI32 -> ()
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
			if not (is_dynamic (rtype r)) then error (reg_inf r ^ " should be castable to dynamic")
		in
		let tfield o fid proto =
			if fid < 0 then error (reg_inf o ^ " does not have " ^ (if proto then "proto " else "") ^ "field " ^ string_of_int fid);
			match rtype o with
			| HObj p ->
				if proto then ftypes.(p.pvirtuals.(fid)) else (try snd (resolve_field p fid) with Not_found -> error (reg_inf o ^ " does not have field " ^ string_of_int fid))
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
			| OString (r,i) | OBytes (r,i) ->
				reg r HBytes;
				if i < 0 || i >= Array.length code.strings then error "string outside range";
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
				| HFun (tobj :: targs, tret) when List.length targs = List.length rl -> reg 0 tobj; List.iter2 reg rl targs; reg r tret
				| t -> check t (HFun (rtype 0 :: List.map rtype rl, rtype r)));
			| OCallMethod (r, m, rl) ->
				(match rl with
				| [] -> assert false
				| obj :: rl2 ->
					let t, rl = (match rtype obj with
						| HVirtual v ->
							let _, _, t = v.vfields.(m) in
							t, rl2
						| _ ->
							tfield obj m true, rl
					) in
					match t with
					| HFun (targs, tret) when List.length targs = List.length rl -> List.iter2 reg rl targs; reg r tret
					| t -> check t (HFun (List.map rtype rl, rtype r)))
			| OCallClosure (r,f,rl) ->
				(match rtype f with
				| HFun (targs,tret) when List.length targs = List.length rl -> List.iter2 reg rl targs; reg r tret
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
				ignore(rtype r);
				can_jump delta
			| OJUGte (a,b,delta) | OJULt (a,b,delta) | OJSGte (a,b,delta) | OJSLt (a,b,delta) | OJSGt (a,b,delta) | OJSLte (a,b,delta) ->
				reg a (rtype b);
				reg b (rtype a);
				can_jump delta
			| OJEq (a,b,delta) | OJNotEq (a,b,delta) ->
				(match rtype a, rtype b with
				| (HObj _ | HVirtual _), (HObj _ | HVirtual _) -> ()
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
				| HDynObj | HVirtual _ -> ()
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
						assert false)
				| HVirtual v ->
					let _,_, t = v.vfields.(fid) in
					reg r t;
				| _ ->
					is_obj o)
			| OInstanceClosure (r,f,arg) ->
				(match ftypes.(f) with
				| HFun (t :: tl, tret) ->
					reg arg t;
					if not (is_nullable t) then error (reg_inf r ^ " should be nullable");
					reg r (HFun (tl,tret));
				| _ -> assert false);
			| OThrow r ->
				reg r HDyn
			| ORethrow r ->
				reg r HDyn
			| OGetArray (v,a,i) ->
				reg a HArray;
				reg i HI32;
				ignore(rtype v);
			| OGetI8 (r,b,p) ->
				reg r HI32;
				reg b HBytes;
				reg p HI32;
			| OGetI32 (r,b,p) | OGetI16(r,b,p) ->
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
			| OSetI32 (r,p,v) | OSetI16 (r,p,v) ->
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
				| HObj _ | HDynObj | HDyn -> ()
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
				can_jump eend
			| ONullCheck r ->
				ignore(rtype r)
			| OTrap (r, idx) ->
				reg r HDyn;
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
