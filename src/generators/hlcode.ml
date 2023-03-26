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

type reg = int
type global = int
type 'a index = int
type functable

type ttype =
	| HVoid
	| HUI8
	| HUI16
	| HI32
	| HI64
	| HF32
	| HF64
	| HBool
	| HBytes
	| HDyn
	| HFun of ttype list * ttype
	| HObj of class_proto
	| HArray
	| HType
	| HRef of ttype
	| HVirtual of virtual_proto
	| HDynObj
	| HAbstract of string * string index
	| HEnum of enum_proto
	| HNull of ttype
	| HMethod of ttype list * ttype
	| HStruct of class_proto
	| HPacked of ttype

and class_proto = {
	pname : string;
	pid : int;
	mutable pclassglobal : int option;
	mutable psuper : class_proto option;
	mutable pvirtuals : int array;
	mutable pproto : field_proto array;
	mutable pnfields : int;
	mutable pfields : (string * string index * ttype) array;
	mutable pindex : (string, int * ttype) PMap.t;
	mutable pfunctions : (string, int) PMap.t;
	mutable pinterfaces : (ttype, int) PMap.t;
	mutable pbindings : (int * int) list;
}

and enum_proto = {
	ename : string;
	eid : int;
	mutable eglobal : int option;
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
	(* register storing *)
	| OMov of reg * reg
	| OInt of reg * int index
	| OFloat of reg * float index
	| OBool of reg * bool
	| OBytes of reg * string index
	| OString of reg * string index
	| ONull of reg
	(* binops *)
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
	(* unops *)
	| ONeg of reg * reg
	| ONot of reg * reg
	| OIncr of reg
	| ODecr of reg
	(* calls *)
	| OCall0 of reg * functable index
	| OCall1 of reg * functable index * reg
	| OCall2 of reg * functable index * reg * reg
	| OCall3 of reg * functable index * reg * reg * reg
	| OCall4 of reg * functable index * reg * reg * reg * reg
	| OCallN of reg * functable index * reg list
	| OCallMethod of reg * field index * reg list
	| OCallThis of reg * field index * reg list
	| OCallClosure of reg * reg * reg list
	(* closures *)
	| OStaticClosure of reg * functable index (* Class.method *)
	| OInstanceClosure of reg * functable index * reg (* instance.method *)
	| OVirtualClosure of reg * reg * field index (* instance.overriddenMethod *)
	(* field access *)
	| OGetGlobal of reg * global
	| OSetGlobal of global * reg
	| OField of reg * reg * field index
	| OSetField of reg * field index * reg
	| OGetThis of reg * field index
	| OSetThis of field index * reg
	| ODynGet of reg * reg * string index
	| ODynSet of reg * string index * reg
	(* jumps *)
	| OJTrue of reg * int
	| OJFalse of reg * int
	| OJNull of reg * int
	| OJNotNull of reg * int
	| OJSLt of reg * reg * int
	| OJSGte of reg * reg * int
	| OJSGt of reg * reg * int
	| OJSLte of reg * reg * int
	| OJULt of reg * reg * int
	| OJUGte of reg * reg * int
	| OJNotLt of reg * reg * int
	| OJNotGte of reg * reg * int
	| OJEq of reg * reg * int
	| OJNotEq of reg * reg * int
	| OJAlways of int
	(* coerce *)
	| OToDyn of reg * reg
	| OToSFloat of reg * reg
	| OToUFloat of reg * reg
	| OToInt of reg * reg
	| OSafeCast of reg * reg
	| OUnsafeCast of reg * reg
	| OToVirtual of reg * reg
	(* control flow *)
	| OLabel of unused
	| ORet of reg
	| OThrow of reg
	| ORethrow of reg
	| OSwitch of reg * int array * int
	| ONullCheck of reg
	| OTrap of reg * int
	| OEndTrap of bool
	(* memory access *)
	| OGetUI8 of reg * reg * reg
	| OGetUI16 of reg * reg * reg
	| OGetMem of reg * reg * reg
	| OGetArray of reg * reg * reg
	| OSetUI8 of reg * reg * reg
	| OSetUI16 of reg * reg * reg
	| OSetMem of reg * reg * reg
	| OSetArray of reg * reg * reg
	(* type operations *)
	| ONew of reg
	| OArraySize of reg * reg
	| OType of reg * ttype
	| OGetType of reg * reg
	| OGetTID of reg * reg
	(* references *)
	| ORef of reg * reg
	| OUnref of reg * reg
	| OSetref of reg * reg
	(* enums *)
	| OMakeEnum of reg * field index * reg list
	| OEnumAlloc of reg * field index
	| OEnumIndex of reg * reg
	| OEnumField of reg * reg * field index * int
	| OSetEnumField of reg * int * reg
	(* misc *)
	| OAssert of unused
	| ORefData of reg * reg
	| ORefOffset of reg * reg * reg
	| ONop of string

type fundecl = {
	fpath : string * string;
	findex : functable index;
	ftype : ttype;
	regs : ttype array;
	code : opcode array;
	debug : (int * int) array;
	assigns : (string index * int) array;
}

type code = {
	version : int;
	entrypoint : global;
	strings : string array;
	ints : int32 array;
	floats : float array;
	bytes : bytes array;
	(* types : ttype array // only in bytecode, rebuilt on save() *)
	globals : ttype array;
	natives : (string index * string index * ttype * functable index) array;
	functions : fundecl array;
	debugfiles : string array;
	constants : (global * int array) array;
}

let null_proto =
	{
		pname = "";
		pid = 0;
		pclassglobal = None;
		psuper = None;
		pvirtuals = [||];
		pproto = [||];
		pfields = [||];
		pnfields = 0;
		pindex = PMap.empty;
		pfunctions = PMap.empty;
		pinterfaces = PMap.empty;
		pbindings = [];
	}

let list_iteri f l =
	let p = ref (-1) in
	List.iter (fun v -> incr p; f !p v) l

let list_mapi f l =
	let p = ref (-1) in
	List.map (fun v -> incr p; f !p v) l

(*
	does the runtime value can be set to null
*)
let is_nullable t =
	match t with
	| HBytes | HDyn | HFun _ | HObj _ | HArray | HVirtual _ | HDynObj | HAbstract _ | HEnum _ | HNull _ | HRef _ | HType | HMethod _ | HStruct _ -> true
	| HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64 | HBool | HVoid | HPacked _ -> false

let is_struct = function
	| HStruct _ | HPacked _ -> true
	| _ -> false

let is_int = function
	| HUI8 | HUI16 | HI32 | HI64 -> true
	| _ -> false

let is_float = function
	| HF32 | HF64 -> true
	| _ -> false

let is_number = function
	| HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64 -> true
	| _ -> false

(*
	does the runtime value carry its type
*)
let is_dynamic t =
	match t with
	| HDyn | HFun _ | HObj _ | HArray | HVirtual _ | HDynObj | HNull _ | HEnum _ -> true
	| _ -> false

let rec tsame t1 t2 =
	if t1 == t2 then true else
	match t1, t2 with
	| HFun (args1,ret1), HFun (args2,ret2) when List.length args1 = List.length args2 -> List.for_all2 tsame args1 args2 && tsame ret2 ret1
	| HMethod (args1,ret1), HMethod (args2,ret2) when List.length args1 = List.length args2 -> List.for_all2 tsame args1 args2 && tsame ret2 ret1
	| HObj p1, HObj p2 -> p1 == p2
	| HEnum e1, HEnum e2 -> e1 == e2
	| HStruct p1, HStruct p2 -> p1 == p2
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
	| HNull t1, HNull t2 -> tsame t1 t2
	| HRef t1, HRef t2 -> tsame t1 t2
	| _ -> false

(*
	can we use a value of t1 as t2
*)
let rec safe_cast t1 t2 =
	if t1 == t2 then true else
	match t1, t2 with
	| _, HDyn -> is_dynamic t1
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
	| HStruct p1, HStruct p2 ->
		(* allow subtyping *)
		let rec loop p =
			p.pname = p2.pname || (match p.psuper with None -> false | Some p -> loop p)
		in
		loop p1
	| HPacked t1, HStruct _ ->
		safe_cast t1 t2
	| HFun (args1,t1), HFun (args2,t2) when List.length args1 = List.length args2 ->
		List.for_all2 (fun t1 t2 -> safe_cast t2 t1 || (t1 = HDyn && is_dynamic t2)) args1 args2 && safe_cast t1 t2
	| _ ->
		tsame t1 t2

let hl_hash b =
	let h = ref Int32.zero in
	let rec loop i =
		let c = if i = String.length b then 0 else int_of_char b.[i] in
		if c <> 0 then begin
			h := Int32.add (Int32.mul !h 223l) (Int32.of_int c);
			loop (i + 1)
		end else
			Int32.rem !h 0x1FFFFF7Bl
	in
	loop 0

let rec get_index name p =
	try
		PMap.find name p.pindex
	with Not_found ->
		match p.psuper with
		| None -> raise Not_found
		| Some p -> get_index name p

let resolve_field p fid =
	let rec loop pl p =
		let pl = p :: pl in
		match p.psuper with
		| None ->
			let rec fetch id = function
				| [] -> raise Not_found
				| p :: pl ->
					let d = id - Array.length p.pfields in
					if d < 0 then
						let name, _, t = p.pfields.(id) in
						name, t
					else
						fetch d pl
			in
			fetch fid pl
		| Some p ->
			loop pl p
	in
	loop [] p

let gather_types (code:code) =
	let types = ref PMap.empty in
	let arr = DynArray.create() in
	let rec get_type t =
		(match t with
		| HObj { psuper = Some p } -> get_type (HObj p)
		| HStruct { psuper = Some p } -> get_type (HStruct p)
		| _ -> ());
		if PMap.mem t !types then () else
		let index = DynArray.length arr in
		DynArray.add arr t;
		types := PMap.add t index !types;
		match t with
		| HFun (args, ret) | HMethod (args, ret) ->
			List.iter get_type args;
			get_type ret
		| HObj p | HStruct p ->
			Array.iter (fun (_,n,t) -> get_type t) p.pfields
		| HNull t | HRef t ->
			get_type t
		| HVirtual v ->
			Array.iter (fun (_,_,t) -> get_type t) v.vfields
		| HEnum e ->
			Array.iter (fun (_,_,tl) -> Array.iter get_type tl) e.efields
		| _ ->
			()
	in
	List.iter (fun t -> get_type t) [HVoid; HUI8; HUI16; HI32; HI64; HF32; HF64; HBool; HType; HDyn]; (* make sure all basic types get lower indexes *)
	Array.iter (fun g -> get_type g) code.globals;
	Array.iter (fun (_,_,t,_) -> get_type t) code.natives;
	Array.iter (fun f ->
		get_type f.ftype;
		Array.iter (fun r -> get_type r) f.regs;
		Array.iter (function
			| OType (_,t) -> get_type t
			| _ -> ()
		) f.code;
	) code.functions;
	DynArray.to_array arr, !types

let lookup_type types t =
	try PMap.find t types with Not_found -> Globals.die "" __LOC__

(* --------------------------------------------------------------------------------------------------------------------- *)
(* DUMP *)

let rec tstr ?(stack=[]) ?(detailed=false) t =
	match t with
	| HVoid -> "void"
	| HUI8 -> "ui8"
	| HUI16 -> "ui16"
	| HI32 -> "i32"
	| HI64 -> "i64"
	| HF32 -> "f32"
	| HF64 -> "f64"
	| HBool -> "bool"
	| HBytes -> "bytes"
	| HDyn  -> "dyn"
	| HFun (args,ret) -> "(" ^ String.concat "," (List.map (tstr ~stack ~detailed) args) ^ "):" ^ tstr ~stack ~detailed ret
	| HMethod (args,ret) -> "method:(" ^ String.concat "," (List.map (tstr ~stack ~detailed) args) ^ "):" ^ tstr ~stack ~detailed ret
	| HObj o when not detailed -> o.pname
	| HStruct s when not detailed -> "@" ^ s.pname
	| HObj o | HStruct o ->
		let fields = "{" ^ String.concat "," (List.map (fun(s,_,t) -> s ^ " : " ^ tstr ~detailed:false t) (Array.to_list o.pfields)) ^ "}" in
		let proto = "{"  ^ String.concat "," (List.map (fun p -> (match p.fvirtual with None -> "" | Some _ -> "virtual ") ^ p.fname ^ "@" ^  string_of_int p.fmethod) (Array.to_list o.pproto)) ^ "}" in
		let str = o.pname ^ "[" ^ (match o.psuper with None -> "" | Some p -> ">" ^ p.pname ^ " ") ^ "fields=" ^ fields ^ " proto=" ^ proto ^ "]" in
		(match t with HObj o -> str | _ -> "@" ^ str)
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
	| HNull t -> "null(" ^ tstr t ^ ")"
	| HPacked t -> "packed(" ^ tstr t ^ ")"

let ostr fstr i o =
	let jump_target d = Printf.sprintf "@%X" (i + d + 1) in
	match o with
	| OMov (a,b) -> Printf.sprintf "mov %d,%d" a b
	| OInt (r,i) -> Printf.sprintf "int %d,@%d" r i
	| OFloat (r,i) -> Printf.sprintf "float %d,@%d" r i
	| OString (r,s) -> Printf.sprintf "string %d,@%d" r s
	| OBytes (r,s) -> Printf.sprintf "bytes %d,@%d" r s
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
	| OCall0 (r,g) -> Printf.sprintf "call %d, %s()" r (fstr g)
	| OCall1 (r,g,a) -> Printf.sprintf "call %d, %s(%d)" r (fstr g) a
	| OCall2 (r,g,a,b) -> Printf.sprintf "call %d, %s(%d,%d)" r (fstr g) a b
	| OCall3 (r,g,a,b,c) -> Printf.sprintf "call %d, %s(%d,%d,%d)" r (fstr g) a b c
	| OCall4 (r,g,a,b,c,d) -> Printf.sprintf "call %d, %s(%d,%d,%d,%d)" r (fstr g) a b c d
	| OCallN (r,g,rl) -> Printf.sprintf "call %d, %s(%s)" r (fstr g) (String.concat "," (List.map string_of_int rl))
	| OCallMethod (r,f,[]) -> "callmethod ???"
	| OCallMethod (r,f,o :: rl) -> Printf.sprintf "callmethod %d, %d[%d](%s)" r o f (String.concat "," (List.map string_of_int rl))
	| OCallClosure (r,f,rl) -> Printf.sprintf "callclosure %d, %d(%s)" r f (String.concat "," (List.map string_of_int rl))
	| OCallThis (r,f,rl) -> Printf.sprintf "callthis %d, [%d](%s)" r f (String.concat "," (List.map string_of_int rl))
	| OStaticClosure (r,f) -> Printf.sprintf "staticclosure %d, %s" r (fstr f)
	| OInstanceClosure (r,f,v) -> Printf.sprintf "instanceclosure %d, %s(%d)" r (fstr f) v
	| OGetGlobal (r,g) -> Printf.sprintf "global %d, %d" r g
	| OSetGlobal (g,r) -> Printf.sprintf "setglobal %d, %d" g r
	| ORet r -> Printf.sprintf "ret %d" r
	| OJTrue (r,d) -> Printf.sprintf "jtrue %d,%s" r (jump_target d)
	| OJFalse (r,d) -> Printf.sprintf "jfalse %d,%s" r (jump_target d)
	| OJNull (r,d) -> Printf.sprintf "jnull %d,%s" r (jump_target d)
	| OJNotNull (r,d) -> Printf.sprintf "jnotnull %d,%s" r (jump_target d)
	| OJSLt (a,b,i) -> Printf.sprintf "jslt %d,%d,%s" a b (jump_target i)
	| OJSGte (a,b,i) -> Printf.sprintf "jsgte %d,%d,%s" a b (jump_target i)
	| OJSGt (r,a,b) -> Printf.sprintf "jsgt %d,%d,%s" r a (jump_target b)
	| OJSLte (r,a,b) -> Printf.sprintf "jslte %d,%d,%s" r a (jump_target b)
	| OJULt (a,b,i) -> Printf.sprintf "jult %d,%d,%s" a b (jump_target i)
	| OJUGte (a,b,i) -> Printf.sprintf "jugte %d,%d,%s" a b (jump_target i)
	| OJNotLt (a,b,i) -> Printf.sprintf "jnotlt %d,%d,%s" a b (jump_target i)
	| OJNotGte (a,b,i) -> Printf.sprintf "jnotgte %d,%d,%s" a b (jump_target i)
	| OJEq (a,b,i) -> Printf.sprintf "jeq %d,%d,%s" a b (jump_target i)
	| OJNotEq (a,b,i) -> Printf.sprintf "jnoteq %d,%d,%s" a b (jump_target i)
	| OJAlways d -> Printf.sprintf "jalways %s" (jump_target d)
	| OToDyn (r,a) -> Printf.sprintf "todyn %d,%d" r a
	| OToSFloat (r,a) -> Printf.sprintf "tosfloat %d,%d" r a
	| OToUFloat (r,a) -> Printf.sprintf "toufloat %d,%d" r a
	| OToInt (r,a) -> Printf.sprintf "toint %d,%d" r a
	| OLabel _ -> "label"
	| ONew r -> Printf.sprintf "new %d" r
	| OField (r,o,i) -> Printf.sprintf "field %d,%d[%d]" r o i
	| OVirtualClosure (r,o,m) -> Printf.sprintf "virtualclosure %d,%d[%d]" r o m
	| OSetField (o,i,r) -> Printf.sprintf "setfield %d[%d],%d" o i r
	| OGetThis (r,i) -> Printf.sprintf "getthis %d,[%d]" r i
	| OSetThis (i,r) -> Printf.sprintf "setthis [%d],%d" i r
	| OThrow r -> Printf.sprintf "throw %d" r
	| ORethrow r -> Printf.sprintf "rethrow %d" r
	| OGetUI8 (r,b,p) -> Printf.sprintf "getui8 %d,%d[%d]" r b p
	| OGetUI16 (r,b,p) -> Printf.sprintf "getui16 %d,%d[%d]" r b p
	| OGetMem (r,b,p) -> Printf.sprintf "getmem %d,%d[%d]" r b p
	| OGetArray (r,a,i) -> Printf.sprintf "getarray %d,%d[%d]" r a i
	| OSetUI8 (r,p,v) -> Printf.sprintf "setui8 %d,%d,%d" r p v
	| OSetUI16 (r,p,v) -> Printf.sprintf "setui16 %d,%d,%d" r p v
	| OSetMem (r,p,v) -> Printf.sprintf "setmem %d,%d,%d" r p v
	| OSetArray (a,i,v) -> Printf.sprintf "setarray %d[%d],%d" a i v
	| OSafeCast (r,v) -> Printf.sprintf "safecast %d,%d" r v
	| OUnsafeCast (r,v) -> Printf.sprintf "unsafecast %d,%d" r v
	| OArraySize (r,a) -> Printf.sprintf "arraysize %d,%d" r a
	| OType (r,t) -> Printf.sprintf "type %d,%s" r (tstr t)
	| OGetType (r,v) -> Printf.sprintf "gettype %d,%d" r v
	| OGetTID (r,v) -> Printf.sprintf "gettid %d,%d" r v
	| ORef (r,v) -> Printf.sprintf "ref %d,&%d" r v
	| OUnref (v,r) -> Printf.sprintf "unref %d,*%d" v r
	| OSetref (r,v) -> Printf.sprintf "setref *%d,%d" r v
	| OToVirtual (r,v) -> Printf.sprintf "tovirtual %d,%d" r v
	| ODynGet (r,o,f) -> Printf.sprintf "dynget %d,%d[@%d]" r o f
	| ODynSet (o,f,v) -> Printf.sprintf "dynset %d[@%d],%d" o f v
	| OMakeEnum (r,e,pl) -> Printf.sprintf "makeenum %d, %d(%s)" r e (String.concat "," (List.map string_of_int pl))
	| OEnumAlloc (r,e) -> Printf.sprintf "enumalloc %d, %d" r e
	| OEnumIndex (r,e) -> Printf.sprintf "enumindex %d, %d" r e
	| OEnumField (r,e,i,n) -> Printf.sprintf "enumfield %d, %d[%d:%d]" r e i n
	| OSetEnumField (e,i,r) -> Printf.sprintf "setenumfield %d[%d], %d" e i r
	| OSwitch (r,idx,eend) -> Printf.sprintf "switch %d [%s] %d" r (String.concat "," (Array.to_list (Array.map string_of_int idx))) eend
	| ONullCheck r -> Printf.sprintf "nullcheck %d" r
	| OTrap (r,i) -> Printf.sprintf "trap %d, %d" r i
	| OEndTrap b -> Printf.sprintf "endtrap %b" b
	| OAssert _ -> "assert"
	| ORefData (r,d) -> Printf.sprintf "refdata %d, %d" r d
	| ORefOffset (r,r2,off) -> Printf.sprintf "refoffset %d, %d, %d" r r2 off
	| ONop s -> if s = "" then "nop" else "nop " ^ s
let fundecl_name f = if snd f.fpath = "" then "fun$" ^ (string_of_int f.findex) else (fst f.fpath) ^ "." ^ (snd f.fpath)

let dump pr code =
	let all_protos = Hashtbl.create 0 in
	let funnames = Hashtbl.create 0 in
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
	let fstr fid =
		try
			Hashtbl.find funnames fid
		with _ ->
			Printf.sprintf "f@%X" fid
	in
	let debug_infos (fid,line) =
		(try code.debugfiles.(fid) with _ -> "???") ^ ":" ^ string_of_int line
	in
	pr ("hl v" ^ string_of_int code.version);
	pr ("entry @" ^ string_of_int code.entrypoint);
	pr (string_of_int (Array.length code.strings) ^ " strings");
	Array.iteri (fun i s ->
		pr ("	@" ^ string_of_int i ^ " : " ^ String.escaped s);
	) code.strings;
	pr (string_of_int (Array.length code.bytes) ^ " bytes");
	Array.iteri (fun i s ->
		pr ("	@" ^ string_of_int i ^ " : " ^ string_of_int (Bytes.length s));
	) code.bytes;
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
		Hashtbl.add funnames fidx (str lib ^ "@" ^ str name)
	) code.natives;
	Array.iter (fun f -> Hashtbl.add funnames f.findex (fundecl_name f)) code.functions;
	pr (string_of_int (Array.length code.functions) ^ " functions");
	Array.iter (fun f ->
		pr (Printf.sprintf "	fun@%d(%Xh) %s" f.findex f.findex (tstr f.ftype));
		let fid, _ = f.debug.(0) in
		let cur_fid = ref fid in
		pr (Printf.sprintf "	; %s (%s)" (debug_infos f.debug.(0)) (fundecl_name f));
		Array.iteri (fun i r ->
			pr ("		r" ^ string_of_int i ^ " " ^ tstr r);
		) f.regs;
		let max_istr = String.length (Printf.sprintf "@%X" (Array.length f.code)) in
		Array.iteri (fun i o ->
			let fid, line = f.debug.(i) in
			if fid <> !cur_fid then begin
				cur_fid := fid;
				pr (Printf.sprintf "	; %s" (debug_infos (fid,line)));
			end;
			let istr = Printf.sprintf "@%X" i in
			pr (Printf.sprintf "		.%-5d %*s %s" line max_istr istr (ostr fstr i o))
		) f.code;
	) code.functions;
	let protos = Hashtbl.fold (fun _ p acc -> p :: acc) all_protos [] in
	pr (string_of_int (List.length protos) ^ " objects protos");
	List.iter (fun p ->
		pr ("	" ^ p.pname ^ " " ^ (match p.pclassglobal with None -> "no global" | Some i -> "@" ^ string_of_int i));
		(match p.psuper with
		| None -> ()
		| Some p -> pr ("		extends " ^ p.pname));
		pr ("		" ^ string_of_int (Array.length p.pfields) ^ " fields");
		let rec loop = function
			| None -> 0
			| Some p -> Array.length p.pfields + loop p.psuper
		in
		let start_field = loop p.psuper in
		Array.iteri (fun i (_,id,t) ->
			pr ("		  @" ^ string_of_int (i + start_field) ^ " " ^ str id ^ " " ^ tstr t)
		) p.pfields;
		pr ("		" ^ string_of_int (Array.length p.pproto) ^ " methods");
		Array.iteri (fun i f ->
			pr ("		  @" ^ string_of_int i ^ " " ^ str f.fid ^ " fun@" ^ string_of_int f.fmethod ^ (match f.fvirtual with None -> "" | Some p -> "[" ^ string_of_int p ^ "]"))
		) p.pproto;
		pr ("		" ^ string_of_int (List.length p.pbindings) ^ " bindings");
		List.iter (fun (i,fidx) ->
			pr ("		  @" ^ string_of_int i ^ " fun@" ^ string_of_int fidx)
		) p.pbindings;
	) protos;
	pr (string_of_int (Array.length code.constants) ^ " constant values");
	Array.iter (fun (g,fields) ->
		pr (Printf.sprintf "    @%d %s [%s]" g (tstr code.globals.(g)) (String.concat "," (List.map string_of_int (Array.to_list fields))));
	) code.constants
