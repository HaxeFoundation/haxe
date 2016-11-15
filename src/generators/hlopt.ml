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
open Hlcode

module ISet = Set.Make(struct
	let compare = Pervasives.compare
	type t = int
end)

type cur_value =
	| VUndef
	| VReg of int

type reg_state = {
	mutable rindex : int;
	mutable ralias : reg_state;
	mutable rbind : reg_state list;
	mutable rnullcheck : bool;
}

type block = {
	bstart : int;
	mutable bend : int;
	mutable bnext : block list;
	mutable bprev : block list;
	mutable bloop : bool;
	mutable bstate : reg_state array option;
	mutable bneed : ISet.t;
	mutable bneed_all : ISet.t option;
	mutable bwrite : (int, int) PMap.t;
}

type control =
	| CNo
	| CJCond of int
	| CJAlways of int
	| CTry of int
	| CSwitch of int array
	| CRet
	| CThrow
	| CLabel

let control = function
	| OJTrue (_,d) | OJFalse (_,d) | OJNull (_,d) | OJNotNull (_,d)
	| OJSLt (_,_,d) | OJSGte (_,_,d) | OJSGt (_,_,d) | OJSLte (_,_,d) | OJULt (_,_,d) | OJUGte (_,_,d) | OJEq (_,_,d) | OJNotEq (_,_,d) ->
		CJCond d
	| OJAlways d ->
		CJAlways d
	| OLabel _ ->
		CLabel
	| ORet _ ->
		CRet
	| OThrow _ | ORethrow _ ->
		CThrow
	| OSwitch (_,cases,_) ->
		CSwitch cases
	| OTrap (_,d) ->
		CTry d
	| _ ->
		CNo

let opcode_fx frw op =
	let read r = frw r true and write r = frw r false in
	match op with
	| OMov (d,a) | ONeg (d,a) | ONot (d,a) ->
		read a; write d
	| OInt (d,_) | OFloat (d,_) | OBool (d,_) | OBytes (d,_) | OString (d,_) | ONull d ->
		write d
	| OAdd (d,a,b) | OSub (d,a,b) | OMul (d,a,b) | OSDiv (d,a,b) | OUDiv (d,a,b) | OSMod (d,a,b)| OUMod (d,a,b) | OShl (d,a,b) | OSShr (d,a,b) | OUShr (d,a,b) | OAnd (d,a,b) | OOr (d,a,b) | OXor (d,a,b) ->
		read a; read b; write d
	| OIncr a | ODecr a ->
		read a; write a
	| OCall0 (d,_) ->
		write d
	| OCall1 (d,_,a) ->
		read a; write d
	| OCall2 (d,_,a,b) ->
		read a; read b; write d
	| OCall3 (d,_,a,b,c) ->
		read a; read b; read c; write d
	| OCall4 (d,_,a,b,c,k) ->
		read a; read b; read c; read k; write d
	| OCallN (d,_,rl) | OCallMethod (d,_,rl) | OCallThis (d,_,rl) ->
		List.iter read rl; write d
	| OCallClosure (d,f,rl) ->
		read f; List.iter read rl; write d
	| OStaticClosure (d,_) ->
		write d
	| OInstanceClosure (d, _, a) | OVirtualClosure (d,a,_) ->
		read a; write d
	| OGetGlobal (d,_) ->
		write d
	| OSetGlobal (_,a) ->
		read a;
	| OSetMethod (o,_,_) ->
		read o;
	| OField (d,a,_) | ODynGet (d,a,_) ->
		read a; write d
	| OSetField (a,_,b) | ODynSet (a,_,b)->
		read a; read b
	| OGetThis (d,_) ->
		write d
	| OSetThis (_,a) ->
		read a
	| OJTrue (r,_) | OJFalse (r,_) | OJNull (r,_) | OJNotNull (r,_) ->
		read r
	| OJSLt (a,b,_) | OJSGte (a,b,_) | OJSGt (a,b,_) | OJSLte (a,b,_) | OJULt (a,b,_) | OJUGte (a,b,_) | OJEq (a,b,_) | OJNotEq (a,b,_) ->
		read a; read b;
	| OJAlways _ | OLabel _ ->
		()
	| OToDyn (d, a) | OToSFloat (d,a) | OToUFloat (d,a) | OToInt (d,a) | OSafeCast (d,a) | OUnsafeCast (d,a) | OToVirtual (d,a) ->
		read a; write d
	| ORet r | OThrow r  | ORethrow r | OSwitch (r,_,_) | ONullCheck r ->
		read r
	| OTrap (r,_) ->
		write r
	| OEndTrap _ ->
		() (* ??? *)
	| OGetUI8 (d,a,b) | OGetUI16 (d,a,b) | OGetI32 (d,a,b) | OGetF32 (d,a,b) | OGetF64 (d,a,b) | OGetArray (d,a,b) ->
		read a; read b; write d
	| OSetUI8 (a,b,c) | OSetUI16 (a,b,c) | OSetI32 (a,b,c) | OSetF32 (a,b,c) | OSetF64 (a,b,c) | OSetArray (a,b,c) ->
		read a; read b; read c
	| ONew d ->
		write d
	| OArraySize (d, a)	| OGetType (d,a) | OGetTID (d,a) | ORef (d, a) | OUnref (d,a) | OSetref (d, a) | OEnumIndex (d, a) | OEnumField (d,a,_,_) ->
		read a;
		write d
	| OType (d,_) | OEnumAlloc (d,_) ->
		write d
	| OMakeEnum (d,_,rl) ->
		List.iter read rl;
		write d
	| OSetEnumField (a,_,b) ->
		read a; read b
	| ONop _ ->
		()

let opcode_eq a b =
	match a, b with
	| OType (r1,t1), OType (r2,t2) ->
		r1 = r2 && t1 == t2
	| _ ->
		a = b

let opcode_map read write op =
	match op with
	| OMov (d,a) ->
		let a = read a in
		OMov (write d, a)
	| ONeg (d,a) ->
		let a = read a in
		ONeg (write d, a)
	| ONot (d,a) ->
		let a = read a in
		ONot (write d, a)
	| OInt (d,idx) ->
		OInt (write d, idx)
	| OFloat (d,idx) ->
		OFloat (write d, idx)
	| OBool (d,idx) ->
		OBool (write d, idx)
	| OBytes (d,idx) ->
		OBytes (write d, idx)
	| OString (d,idx) ->
		OString (write d, idx)
	| ONull d ->
		ONull (write d)
	| OAdd (d,a,b) ->
		let a = read a and b = read b in
		OAdd (write d, a, b)
	| OSub (d,a,b) ->
		let a = read a and b = read b in
		OSub (write d, a, b)
	| OMul (d,a,b) ->
		let a = read a and b = read b in
		OMul (write d, a, b)
	| OSDiv (d,a,b) ->
		let a = read a and b = read b in
		OSDiv (write d, a, b)
	| OUDiv (d,a,b) ->
		let a = read a and b = read b in
		OUDiv (write d, a, b)
	| OSMod (d,a,b) ->
		let a = read a and b = read b in
		OSMod (write d, a, b)
	| OUMod (d,a,b) ->
		let a = read a and b = read b in
		OUMod (write d, a, b)
	| OShl (d,a,b) ->
		let a = read a and b = read b in
		OShl (write d, a, b)
	| OSShr (d,a,b) ->
		let a = read a and b = read b in
		OSShr (write d, a, b)
	| OUShr (d,a,b) ->
		let a = read a and b = read b in
		OUShr (write d, a, b)
	| OAnd (d,a,b) ->
		let a = read a and b = read b in
		OAnd (write d, a, b)
	| OOr (d,a,b) ->
		let a = read a and b = read b in
		OOr (write d, a, b)
	| OXor (d,a,b) ->
		let a = read a and b = read b in
		OXor (write d, a, b)
	| OIncr a ->
		OIncr (write a)
	| ODecr a ->
		ODecr (write a)
	| OCall0 (d,f) ->
		OCall0 (write d, f)
	| OCall1 (d,f,a) ->
		let a = read a in
		OCall1 (write d, f, a)
	| OCall2 (d,f,a,b) ->
		let a = read a in
		let b = read b in
		OCall2 (write d, f, a, b)
	| OCall3 (d,f,a,b,c) ->
		let a = read a in
		let b = read b in
		let c = read c in
		OCall3 (write d, f, a, b, c)
	| OCall4 (w,f,a,b,c,d) ->
		let a = read a in
		let b = read b in
		let c = read c in
		let d = read d in
		OCall4 (write w, f, a, b, c, d)
	| OCallN (d,f,rl) ->
		let rl = List.map read rl in
		OCallN (write d, f, rl)
	| OCallMethod (d,f,rl) ->
		let rl = List.map read rl in
		OCallMethod (write d, f, rl)
	| OCallThis (d,f,rl) ->
		let rl = List.map read rl in
		OCallThis (write d, f, rl)
	| OCallClosure (d,f,rl) ->
		let f = read f in
		let rl = List.map read rl in
		OCallClosure (write d, f, rl)
	| OStaticClosure (d,f) ->
		OStaticClosure (write d, f)
	| OInstanceClosure (d, f, a) ->
		let a = read a in
		OInstanceClosure (write d, f, a)
	| OVirtualClosure (d,a,f) ->
		let a = read a in
		OVirtualClosure (write d, a, f)
	| OGetGlobal (d,g) ->
		OGetGlobal (write d, g)
	| OSetGlobal (g,r) ->
		OSetGlobal (g, read r)
	| OSetMethod (o,f,m) ->
		OSetMethod (read o, f, m)
	| OField (d,a,f) ->
		let a = read a in
		OField (write d, a, f)
	| ODynGet (d,a,f) ->
		let a = read a in
		ODynGet (write d, a, f)
	| OSetField (a,f,b) ->
		OSetField (read a, f, read b)
	| ODynSet (a,f,b) ->
		ODynSet (read a, f, read b)
	| OGetThis (d,f) ->
		OGetThis (write d, f)
	| OSetThis (f,a) ->
		OSetThis (f, read a)
	| OJTrue (r,d) ->
		OJTrue (read r, d)
	| OJFalse (r,d) ->
		OJFalse (read r, d)
	| OJNull (r,d) ->
		OJNull (read r, d)
	| OJNotNull (r,d) ->
		OJNotNull (read r, d)
	| OJSLt (a,b,d) ->
		OJSLt (read a, read b, d)
	| OJSGte (a,b,d) ->
		OJSGte (read a, read b, d)
	| OJSGt (a,b,d) ->
		OJSGt (read a, read b, d)
	| OJSLte (a,b,d) ->
		OJSLte (read a, read b, d)
	| OJULt (a,b,d) ->
		OJULt (read a, read b, d)
	| OJUGte (a,b,d) ->
		OJUGte (read a, read b, d)
	| OJEq (a,b,d) ->
		OJEq (read a, read b, d)
	| OJNotEq (a,b,d) ->
		OJNotEq (read a, read b, d)
	| OJAlways _ | OLabel _ ->
		op
	| OToDyn (d, a) ->
		let a = read a in
		OToDyn (write d, a)
	| OToSFloat (d,a) ->
		let a = read a in
		OToSFloat (write d, a)
	| OToUFloat (d,a) ->
		let a = read a in
		OToUFloat (write d, a)
	| OToInt (d,a) ->
		let a = read a in
		OToInt (write d, a)
	| OSafeCast (d,a) ->
		let a = read a in
		OSafeCast (write d, a)
	| OUnsafeCast (d,a) ->
		let a = read a in
		OUnsafeCast (write d, a)
	| OToVirtual (d,a) ->
		let a = read a in
		OToVirtual (write d, a)
	| ORet r ->
		ORet (read r)
	| OThrow r ->
		OThrow (read r)
	| ORethrow r ->
		ORethrow (read r)
	| OSwitch (r,cases,def) ->
		OSwitch (read r, cases, def)
	| ONullCheck r ->
		ONullCheck (read r)
	| OTrap (r,d) ->
		OTrap (write r, d)
	| OEndTrap _ ->
		op (* ??? *)
	| OGetUI8 (d,a,b) ->
		let a = read a and b = read b in
		OGetUI8 (write d, a, b)
	| OGetUI16 (d,a,b) ->
		let a = read a and b = read b in
		OGetUI16 (write d, a, b)
	| OGetI32 (d,a,b) ->
		let a = read a and b = read b in
		OGetI32 (write d, a, b)
	| OGetF32 (d,a,b) ->
		let a = read a and b = read b in
		OGetF32 (write d, a, b)
	| OGetF64 (d,a,b) ->
		let a = read a and b = read b in
		OGetF64 (write d, a, b)
	| OGetArray (d,a,b) ->
		let a = read a and b = read b in
		OGetArray (write d, a, b)
	| OSetUI8 (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetUI8 (a, b, c)
	| OSetUI16 (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetUI16 (a, b, c)
	| OSetI32 (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetI32 (a, b, c)
	| OSetF32 (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetF32 (a, b, c)
	| OSetF64 (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetF64 (a, b, c)
	| OSetArray (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetArray (a, b, c)
	| ONew d ->
		ONew (write d)
	| OArraySize (d, a) ->
		let a = read a in
		OArraySize (write d, a)
	| OGetType (d,a) ->
		let a = read a in
		OGetType (write d, a)
	| OGetTID (d,a) ->
		let a = read a in
		OGetTID (write d, a)
	| ORef (d, a) ->
		let a = read a in
		ORef (write d, a)
	| OUnref (d,a) ->
		let a = read a in
		OUnref (write d, a)
	| OSetref (d, a) ->
		let a = read a in
		OSetref (write d, a)
	| OEnumIndex (d, a) ->
		let a = read a in
		OEnumIndex (write d, a)
	| OEnumField (d,a,cs,idx) ->
		let a = read a in
		OEnumField (write d, a, cs, idx)
	| OType (d,t) ->
		OType (write d, t)
	| OEnumAlloc (d,e) ->
		OEnumAlloc (write d, e)
	| OMakeEnum (d,e,rl) ->
		let rl = List.map read rl in
		OMakeEnum (write d, e, rl)
	| OSetEnumField (a,f,b) ->
		OSetEnumField (read a, f, read b)
	| ONop _ ->
		op

(* build code graph *)

let code_graph (f:fundecl) =
	let op index = f.code.(index) in
	let blocks_pos = Hashtbl.create 0 in
	let all_blocks = Hashtbl.create 0 in
	for i = 0 to Array.length f.code - 1 do
		match control (op i) with
		| CJAlways d | CJCond d -> Hashtbl.replace all_blocks (i + 1 + d) true
		| _ -> ()
	done;
	let rec make_block pos =
		try
			Hashtbl.find blocks_pos pos
		with Not_found ->
			let b = {
				bstart = pos;
				bend = 0;
				bnext = [];
				bprev = [];
				bloop = false;
				bstate = None;
				bneed = ISet.empty;
				bwrite = PMap.empty;
				bneed_all = None;
			} in
			Hashtbl.add blocks_pos pos b;
			let rec loop i =
				let goto d =
					let b2 = make_block (i + 1 + d) in
					b2.bprev <- b :: b2.bprev;
					b2
				in
				if i > pos && Hashtbl.mem all_blocks i then begin
					b.bend <- i - 1;
					b.bnext <- [goto (-1)];
				end else match control (op i) with
				| CNo ->
					loop (i + 1)
				| CRet | CThrow ->
					b.bend <- i
				| CJAlways d ->
					b.bend <- i;
					b.bnext <- [goto d];
				| CSwitch pl ->
					b.bend <- i;
					b.bnext <- goto 0 :: Array.to_list (Array.map goto pl)
				| CJCond d | CTry d ->
					b.bend <- i;
					b.bnext <- [goto 0; goto d];
				| CLabel ->
					b.bloop <- true;
					loop (i + 1)
			in
			loop pos;
			b
	in
	blocks_pos, make_block 0

let optimize dump (f:fundecl) =
	let nregs = Array.length f.regs in
	let old_code = match dump with None -> f.code | Some _ -> Array.copy f.code in
	let op index = f.code.(index) in
	let set_op index op = f.code.(index) <- op in
	let nop_count = ref 0 in
	let set_nop index r = f.code.(index) <- (ONop r); incr nop_count in
	let write str = match dump with None -> () | Some ch -> IO.nwrite ch (str ^ "\n") in

	let blocks_pos, root = code_graph f in

	let read_counts = Array.make nregs 0 in
	let write_counts = Array.make nregs 0 in
	let last_write = Array.make nregs (-1) in

	let bit_regs = 30 in
	let stride = (nregs + bit_regs - 1) / bit_regs in
	let live_bits = Array.make (Array.length f.code * stride) 0 in

	let set_live r min max =
		let offset = r / bit_regs in
		let mask = 1 lsl (r - offset * bit_regs) in
		if min < 0 || max >= Array.length f.code then assert false;
		for i=min to max do
			let p = i * stride + offset in
			Array.unsafe_set live_bits p ((Array.unsafe_get live_bits p) lor mask);
		done;
	in
	let is_live r i =
		let offset = r / bit_regs in
		let mask = 1 lsl (r - offset * bit_regs) in
		live_bits.(i * stride + offset) land mask <> 0
	in

	let read_count r = read_counts.(r) <- read_counts.(r) + 1 in
	let write_count r = write_counts.(r) <- write_counts.(r) + 1 in

	let empty_state() = Array.init nregs (fun i ->
		let r = { rindex = i; ralias = Obj.magic 0; rbind = []; rnullcheck = false } in
		r.ralias <- r;
		r
	) in

	let print_state i s =
		let state_str s =
			if s.ralias == s && s.rbind == [] then "" else
			Printf.sprintf "%d%s[%s]" s.rindex (if s.ralias == s then "" else "=" ^ string_of_int s.ralias.rindex) (String.concat "," (List.map (fun s -> string_of_int s.rindex) s.rbind))
		in
		write (Printf.sprintf "@%X %s" i (String.concat " " (Array.to_list (Array.map state_str s))))
	in

	let dstate = false in

	let rec propagate b =
		let state = if b.bloop then
			empty_state()
		else match b.bprev with
		| [] -> empty_state()
		| b2 :: l ->
			let s = get_state b2 in
			let s = (match b2.bnext with
			| [] -> assert false
			| [_] -> s (* reuse *)
			| _ :: l ->
				let s2 = empty_state() in
				for i = 0 to nregs - 1 do
					let sold = s.(i) and snew = s2.(i) in
					snew.ralias <- s2.(sold.ralias.rindex);
					snew.rbind <- List.map (fun b -> s2.(b.rindex)) sold.rbind;
					snew.rnullcheck <- sold.rnullcheck;
				done;
				s2
			) in
			List.iter (fun b2 ->
				let s2 = get_state b2 in
				for i = 0 to nregs - 1 do
					let s1 = s.(i) and s2 = s2.(i) in
					if s1.ralias.rindex <> s2.ralias.rindex then s1.ralias <- s1
				done;
				for i = 0 to nregs - 1 do
					let s1 = s.(i) and s2 = s2.(i) in
					s1.rbind <- List.filter (fun s -> s.ralias == s1) s1.rbind;
					s1.rnullcheck <- s1.rnullcheck && s2.rnullcheck;
					(match s2.rbind with
					| [] -> ()
					| l -> s1.rbind <- List.fold_left (fun acc sb2 -> let s = s.(sb2.rindex) in if s.ralias == s1 && not (List.memq s s1.rbind) then s :: acc else acc) s1.rbind s2.rbind)
				done;
			) l;
			s
		in
		let unalias r =
			r.ralias.rbind <- List.filter (fun r2 -> r2 != r) r.ralias.rbind;
			r.ralias <- r
		in
		let rec loop i =
			let do_read r =
				let w = last_write.(r) in
				if w < b.bstart || w > i then begin
					last_write.(r) <- i;
					set_live r b.bstart i;
					b.bneed <- ISet.add r b.bneed;
				end else
					set_live r (w + 1) i;
				read_count r
			in
			let do_write r =
				let s = state.(r) in
				List.iter (fun s2 -> s2.ralias <- s2) s.rbind;
				s.rbind <- [];
				s.rnullcheck <- false;
				last_write.(r) <- i;
				b.bwrite <- PMap.add r i b.bwrite;
				write_count r;
				unalias s
			in
			if i > b.bend then () else
			let op = op i in
			if dstate then print_state i state;
			(match op with
			| OIncr r | ODecr r | ORef (_,r) -> unalias state.(r)
			| OCallClosure (_,r,_) when f.regs.(r) = HDyn && (match f.regs.(state.(r).ralias.rindex) with HFun (_,rt) -> not (is_dynamic rt) | HDyn -> false | _ -> true) -> unalias state.(r) (* Issue3218.hx *)
			| _ -> ());
			let op = opcode_map (fun r ->
				let s = state.(r) in
				s.ralias.rindex
			) (fun w ->	w) op in
			set_op i op;
			(match op with
			| OMov (d, v) when d = v ->
				set_nop i "mov"
			| OMov (d, v) ->
				let sv = state.(v) in
				let sd = state.(d) in
				do_read v;
				do_write d;
				sd.ralias <- sv;
				sd.rnullcheck <- sv.rnullcheck;
				if not (List.memq sd sv.rbind) then sv.rbind <- sd :: sv.rbind;
			| OIncr r | ODecr r ->
				do_read r;
				do_write r;
			| ONullCheck r ->
				let s = state.(r) in
				if s.rnullcheck then set_nop i "nullcheck" else begin do_read r; s.rnullcheck <- true; end;
			| _ ->
				opcode_fx (fun r read ->
					if read then do_read r else do_write r
				) op
			);
			loop (i + 1)
		in
		loop b.bstart;
		b.bstate <- Some state;
		List.iter (fun b2 -> if b2.bstart > b.bstart then ignore (get_state b2)) b.bnext

	and get_state b =
		match b.bstate with
		| None ->
			propagate b;
			get_state b
		| Some state ->
			state
	in
	propagate root;

	(* unreachable code *)

	let rec loop i =
		if i = Array.length f.code then () else
		try
			let b = Hashtbl.find blocks_pos i in
			loop (b.bend + 1)
		with Not_found ->
			(match op i with
			| OEndTrap true -> ()
			| _ -> set_nop i "unreach");
			loop (i + 1)
	in
	loop 0;

	(* liveness *)

	let rec live b =
		match b.bneed_all with
		| Some a -> a
		| None ->
			let need_sub = List.fold_left (fun acc b2 ->
				(* loop : first pass does not recurse, second pass uses cache *)
				if b2.bloop && b2.bstart < b.bstart then (match b2.bneed_all with None -> acc | Some s -> ISet.union acc s) else
				ISet.union acc (live b2)
			) ISet.empty b.bnext in
			let need_sub = ISet.filter (fun r ->
				try
					let w = PMap.find r b.bwrite in
					set_live r (w + 1) b.bend;
					false
				with Not_found ->
					set_live r b.bstart b.bend;
					true
			) need_sub in
			let need = ISet.union b.bneed need_sub in
			b.bneed_all <- Some need;
			if b.bloop then begin
				(*
					if we are a loop, we need a second pass to perform fixed point
					first clear the cache within the loop from backward
					then rebuild the cache to make sure liveness ranges are correctly set
				*)
				let rec clear b2 =
					match b2.bneed_all with
					| Some _ when b2.bstart > b.bstart ->
						b2.bneed_all <- None;
						List.iter clear b2.bprev
					| _ -> ()
				in
				List.iter (fun b2 -> if b2.bstart > b.bstart then clear b2) b.bprev;
				List.iter (fun b -> ignore(live b)) b.bnext;
			end;
			need
	in
	ignore(live root);

	(* nop *)

	for i=0 to Array.length f.code - 1 do
		(match op i with
		| OMov (d,r) when not (is_live d (i + 1)) ->
			let n = read_counts.(r) - 1 in
			read_counts.(r) <- n;
			write_counts.(d) <- write_counts.(d) - 1;
			set_nop i "unused"
		| _ -> ());
	done;

	(* reg map *)

	let used_regs = ref 0 in
	let reg_map = read_counts in
	let nargs = (match f.ftype with HFun (args,_) -> List.length args | _ -> assert false) in
	for i=0 to nregs-1 do
		if read_counts.(i) > 0 || write_counts.(i) > 0 || i < nargs then begin
			reg_map.(i) <- !used_regs;
			incr used_regs;
		end else
			reg_map.(i) <- -1;
	done;
	let reg_remap = !used_regs <> nregs in

	(* done *)
	if dump <> None then begin
		let rec loop i block =
			if i = Array.length f.code then () else
			let block = try
				let b = Hashtbl.find blocks_pos i in
				write (Printf.sprintf "\t----- [%s] (%X)"
					(String.concat "," (List.map (fun b -> Printf.sprintf "%X" b.bstart) b.bnext))
					b.bend
				);
				let need = String.concat "," (List.map string_of_int (ISet.elements b.bneed)) in
				let wr = String.concat " " (List.rev (PMap.foldi (fun r p acc -> Printf.sprintf "r%d:%X" r p :: acc) b.bwrite [])) in
				write ("\tNEED=" ^ need ^ "\tWRITE=" ^ wr);
				b
			with Not_found ->
				block
			in
			let old = old_code.(i) in
			let op = op i in
			let rec live_loop r l =
				if r = nregs then List.rev l else
				live_loop (r + 1) (if is_live r i then r :: l else l)
			in
			let live = "LIVE=" ^ String.concat "," (List.map string_of_int (live_loop 0 [])) in
			write (Printf.sprintf "\t@%-3X %-20s %-20s%s" i (ostr string_of_int old) (if opcode_eq old op then "" else ostr string_of_int op) live);
			loop (i + 1) block
		in
		write (Printf.sprintf "%s@%d" (fundecl_name f) f.findex);
		if reg_remap then begin
			for i=0 to nregs-1 do
				write (Printf.sprintf "\tr%-2d %-10s%s" i (tstr f.regs.(i)) (if reg_map.(i) < 0 then " unused" else if reg_map.(i) = i then "" else Printf.sprintf " r%-2d" reg_map.(i)))
			done;
		end;
		loop 0 root;
		write "";
		write "";
		(match dump with None -> () | Some ch -> IO.flush ch);
	end;

	(* remap *)

	let code = ref f.code in
	let regs = ref f.regs in
	let debug = ref f.debug in

	if !nop_count > 0 || reg_remap then begin
		let new_pos = Array.make (Array.length f.code) 0 in
		let jumps = ref [] in
		let out_pos = ref 0 in
		let out_code = Array.make (Array.length f.code - !nop_count) (ONop "") in
		let new_debug = Array.make (Array.length f.code - !nop_count) (0,0) in
		Array.iteri (fun i op ->
			Array.unsafe_set new_pos i !out_pos;
			match op with
			| ONop _ -> ()
			| _ ->
				(match op with
				| OJTrue _ | OJFalse _ | OJNull _ | OJNotNull _  | OJSLt _ | OJSGte _ | OJSGt _ | OJSLte _ | OJULt _ | OJUGte _ | OJEq _ | OJNotEq _ | OJAlways _ | OSwitch _  | OTrap _ ->
					jumps := i :: !jumps
				| _ -> ());
				let op = if reg_remap then opcode_map (fun r -> reg_map.(r)) (fun r -> reg_map.(r)) op else op in
				out_code.(!out_pos) <- op;
				new_debug.(!out_pos) <- f.debug.(i);
				incr out_pos
		) f.code;
		List.iter (fun j ->
			let pos d =
				new_pos.(j + 1 + d) - new_pos.(j + 1)
			in
			let p = new_pos.(j) in
			out_code.(p) <- (match out_code.(p) with
			| OJTrue (r,d) -> OJTrue (r,pos d)
			| OJFalse (r,d) -> OJFalse (r,pos d)
			| OJNull (r,d) -> OJNull (r, pos d)
			| OJNotNull (r,d) -> OJNotNull (r, pos d)
			| OJSLt (a,b,d) -> OJSLt (a,b,pos d)
			| OJSGte (a,b,d) -> OJSGte (a,b,pos d)
			| OJSLte (a,b,d) -> OJSLte (a,b,pos d)
			| OJSGt (a,b,d) -> OJSGt (a,b,pos d)
			| OJULt (a,b,d) -> OJULt (a,b,pos d)
			| OJUGte (a,b,d) -> OJUGte (a,b,pos d)
			| OJEq (a,b,d) -> OJEq (a,b,pos d)
			| OJNotEq (a,b,d) -> OJNotEq (a,b,pos d)
			| OJAlways d -> OJAlways (pos d)
			| OSwitch (r,cases,send) -> OSwitch (r, Array.map pos cases, pos send)
			| OTrap (r,d) -> OTrap (r,pos d)
			| _ -> assert false)
		) !jumps;
		code := out_code;
		debug := new_debug;
		if reg_remap then begin
			let new_regs = Array.make !used_regs HVoid in
			for i=0 to nregs-1 do
				let p = reg_map.(i) in
				if p >= 0 then new_regs.(p) <- f.regs.(i)
			done;
			regs := new_regs;
		end;
	end;

	{ f with code = !code; regs = !regs; debug = !debug }