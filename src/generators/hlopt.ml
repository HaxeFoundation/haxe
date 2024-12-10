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
open Hlcode

module ISet = Set.Make(struct
	let compare a b = b - a
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
	mutable btrap : int list;
}

type control =
	| CNo
	| CJCond of int
	| CJAlways of int
	| CTry of int
	| CCatch
	| CSwitch of int array
	| CRet
	| CThrow
	| CLabel

let control = function
	| OJTrue (_,d) | OJFalse (_,d) | OJNull (_,d) | OJNotNull (_,d)
	| OJSLt (_,_,d) | OJSGte (_,_,d) | OJSGt (_,_,d) | OJSLte (_,_,d) | OJULt (_,_,d) | OJUGte (_,_,d) | OJEq (_,_,d) | OJNotEq (_,_,d) | OJNotLt (_,_,d) | OJNotGte (_,_,d) ->
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
	| OEndTrap _ ->
		CCatch
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
	| OJSLt (a,b,_) | OJSGte (a,b,_) | OJSGt (a,b,_) | OJSLte (a,b,_) | OJULt (a,b,_) | OJUGte (a,b,_) | OJNotLt (a,b,_) | OJNotGte (a,b,_) | OJEq (a,b,_) | OJNotEq (a,b,_) ->
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
	| OGetUI8 (d,a,b) | OGetUI16 (d,a,b) | OGetMem (d,a,b) | OGetArray (d,a,b) ->
		read a; read b; write d
	| OSetUI8 (a,b,c) | OSetUI16 (a,b,c) | OSetMem (a,b,c) | OSetArray (a,b,c) ->
		read a; read b; read c
	| ONew d ->
		write d
	| OArraySize (d, a)	| OGetType (d,a) | OGetTID (d,a) | OUnref (d,a) | OSetref (d, a) | OEnumIndex (d, a) | OEnumField (d,a,_,_) ->
		read a;
		write d
	| ORef (d, a) ->
		read a;
		write a; (* prevent issue with 'a' being reused later - this is not exact as it can be set everytime we pass it to a function *)
		write d;
	| OType (d,_) | OEnumAlloc (d,_) ->
		write d
	| OMakeEnum (d,_,rl) ->
		List.iter read rl;
		write d
	| OSetEnumField (a,_,b) ->
		read a; read b
	| OAssert _ ->
		()
	| ORefData (r,d) ->
		read d;
		write r;
	| ORefOffset (r,r2,off) ->
		read r2;
		read off;
		write r;
	| ONop _  ->
		()
	| OPrefetch (r,_,_) ->
		read r
    | OAsm (_,_,r) ->
        if r > 0 then begin
            (* assume both *)
            read (r - 1);
            write (r - 1);
        end

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
	| OJNotLt (a,b,d) ->
		OJNotLt (read a, read b, d)
	| OJNotGte (a,b,d) ->
		OJNotGte (read a, read b, d)
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
	| OGetMem (d,a,b) ->
		let a = read a and b = read b in
		OGetMem (write d, a, b)
	| OGetArray (d,a,b) ->
		let a = read a and b = read b in
		OGetArray (write d, a, b)
	| OSetUI8 (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetUI8 (a, b, c)
	| OSetUI16 (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetUI16 (a, b, c)
	| OSetMem (a,b,c) ->
		let a = read a and b = read b and c = read c in
		OSetMem (a, b, c)
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
	| OAssert _ ->
		op
	| ORefData (r,d) ->
		let d = read d in
		ORefData(write r,d);
	| ORefOffset (r,r2,off) ->
		let r2 = read r2 in
		let off = read off in
		ORefOffset (write r,r2,off);
	| ONop _ ->
		op
	| OPrefetch (r, fid, mode) ->
		let r2 = read r in
		OPrefetch (r2, fid, mode)
	| OAsm (_, _, 0) ->
		op
	| OAsm (mode, value, r) ->
		let r2 = read (r - 1) in
		OAsm (mode, value, (write r2) + 1)

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
	let rec make_block trapl pos =
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
				btrap = trapl;
			} in
			Hashtbl.add blocks_pos pos b;
			let rec loop i =
				let goto ?(tl=b.btrap) d =
					let b2 = make_block tl (i + 1 + d) in
					b2.bprev <- b :: b2.bprev;
					b2
				in
				if i > pos && Hashtbl.mem all_blocks i then begin
					b.bend <- i - 1;
					b.bnext <- [goto (-1)];
				end else match control (op i) with
				| CNo ->
					loop (i + 1)
				| CRet ->
					assert(b.btrap = []);
					b.bend <- i
				| CJAlways d ->
					b.bend <- i;
					b.bnext <- [goto d];
				| CSwitch pl ->
					b.bend <- i;
					b.bnext <- goto 0 :: Array.to_list (Array.map goto pl)
				| CJCond d ->
					b.bend <- i;
					b.bnext <- [goto 0; goto d];
				| CTry d ->
					b.bend <- i;
					b.bnext <- [goto ~tl:((i+1+d)::b.btrap) 0; goto d];
				| CThrow ->
					b.bend <- i;
					match b.btrap with
						| [] -> ()
						| [p] -> b.bnext <- [goto ~tl:[] (p-1-i)];
						| p :: pl -> b.bnext <- [goto ~tl:pl (p-1-i)];
					;
				| CCatch ->
					let p, pl = match b.btrap with
						| [] -> assert false;
						| [p] -> p, []
						| p :: pl -> p, pl
					in
					b.bend <- i;
					b.bnext <- [goto ~tl:pl 0; goto ~tl:pl (p-1-i)];
				| CLabel ->
					b.bloop <- true;
					loop (i + 1)
			in
			loop pos;
			b
	in
	blocks_pos, make_block [] 0

type rctx = {
	r_root : block;
	r_used_regs : int;
	r_nop_count : int;
	r_blocks_pos : (int, block) Hashtbl.t;
	r_reg_moved : (int, (int * int)) Hashtbl.t;
	r_live_bits : int array;
	r_reg_map : int array;
}

let remap_fun ctx f dump get_str old_code =
	let op index = Array.unsafe_get f.code index in
	let nregs = Array.length f.regs in
	let reg_remap = ctx.r_used_regs <> nregs in
	let assigns = ref f.assigns in
	let write str = match dump with None -> () | Some ch -> IO.nwrite ch (Bytes.unsafe_of_string (str ^ "\n")) in
	let nargs = (match f.ftype with HFun (args,_) -> List.length args | _ -> Globals.die "" __LOC__) in

	let live_bits = ctx.r_live_bits in
	let reg_map = ctx.r_reg_map in

	let bit_regs = 30 in
	let stride = (nregs + bit_regs - 1) / bit_regs in
	let is_live r i =
		let offset = r / bit_regs in
		let mask = 1 lsl (r - offset * bit_regs) in
		Array.unsafe_get live_bits (i * stride + offset) land mask <> 0
	in

	(* remap assigns *)
	if ctx.r_nop_count > 0 then begin
		let rec resolve_block p =
			try Hashtbl.find ctx.r_blocks_pos p with Not_found -> resolve_block (p - 1)
		in

		let new_assigns = List.fold_left (fun acc (i,p) ->
			let gmap = Hashtbl.create 0 in
			(*
				For a given assign at position p, that's been optimized out,
				let's try to find where the last assign that maps to the same value
				is, and remap the variable name to it
			*)
			let rec loop p =
				if p < 0 || (match op p with ONop _ -> false | _ -> true) then [(i,p)] else
				let reg, last_w = try Hashtbl.find ctx.r_reg_moved p with Not_found -> (-1,-1) in
				if reg < 0 then [] (* ? *) else
				if reg < nargs then [(i,-reg-2)] else
				let b = resolve_block p in
				if last_w >= b.bstart && last_w < b.bend && last_w < p then loop last_w else
				let wp = try PMap.find reg b.bwrite with Not_found -> -1 in
				let rec gather b =
					if Hashtbl.mem gmap b.bstart then [] else begin
						Hashtbl.add gmap b.bstart ();
						(* lookup in all parent blocks, recursively, to fetch all last writes *)
						List.fold_left (fun acc bp ->
							if bp.bstart > b.bstart then acc else
							try
								let wp = PMap.find reg bp.bwrite in
								if wp > p then Globals.die "" __LOC__;
								loop wp @ acc
							with Not_found ->
								gather bp @ acc
						) [] b.bprev;
					end
				in
				if wp < 0 then
					gather b
				else if wp < p then
					loop wp
				else
					(* lookup in writes between p-1 and block bstart *)
					let rec find_w p =
						if p < b.bstart then
							gather b
						else
							let found = ref false in
							opcode_fx (fun r read -> if r = reg && not read then found := true) (Array.unsafe_get old_code p);
							if !found then loop p else find_w (p - 1)
					in
					find_w (p - 1)
			in
			loop p @ acc
		) [] (Array.to_list !assigns) in
		let new_assigns = List.sort (fun (_,p1) (_,p2) -> p1 - p2) (List.rev new_assigns) in
		assigns := Array.of_list new_assigns;
	end;

	(* done *)
	if dump <> None then begin
		let old_assigns = Hashtbl.create 0 in
		let new_assigns = Hashtbl.create 0 in
		Array.iter (fun (var,pos) -> if pos >= 0 then Hashtbl.replace old_assigns pos var) f.assigns;
		Array.iter (fun (var,pos) ->
			if pos >= 0 then begin
				let f = try Hashtbl.find new_assigns pos with Not_found -> let v = ref [] in Hashtbl.add new_assigns pos v; v in
				f := var :: !f;
			end
		) !assigns;
		let rec loop i block =
			if i = Array.length f.code then () else
			let block = try
				let b = Hashtbl.find ctx.r_blocks_pos i in
				write (Printf.sprintf "\t----- [%s] (%X)"
					(String.concat "," (List.map (fun b -> Printf.sprintf "%X" b.bstart) b.bnext))
					b.bend
				);
				let need = String.concat "," (List.map string_of_int (ISet.elements b.bneed)) in
				let wr = String.concat " " (List.rev (PMap.foldi (fun r p acc -> Printf.sprintf "%d@%X" r p :: acc) b.bwrite [])) in
				write ("\t" ^ (if b.bloop then "LOOP " else "") ^ "NEED=" ^ need ^ "\tWRITE=" ^ wr);
				b
			with Not_found ->
				block
			in
			let old = Array.unsafe_get old_code i in
			let op = op i in
			let rec live_loop r l =
				if r = nregs then List.rev l else
				live_loop (r + 1) (if is_live r i then r :: l else l)
			in
			let live = "LIVE=" ^ String.concat "," (List.map string_of_int (live_loop 0 [])) in
			let var_set = (try let v = Hashtbl.find old_assigns i in "set " ^ get_str v with Not_found -> "") in
			let nvar_set = (try let v = Hashtbl.find new_assigns i in "set " ^ String.concat "," (List.map get_str !v) with Not_found -> "") in
			write (Printf.sprintf "\t@%-3X %-20s %-20s %-20s %-20s %s" i (ostr string_of_int old) (if opcode_eq old op then "" else ostr string_of_int op) var_set nvar_set live);
			loop (i + 1) block
		in
		write (Printf.sprintf "%s@%d" (fundecl_name f) f.findex);
		let rec loop_arg = function
			| [] -> []
			| (_,p) :: _ when p >= 0 -> []
			| (str,p) :: l -> (get_str str ^ ":" ^ string_of_int p) :: loop_arg l
		in
		write (Printf.sprintf "ARGS = %s\n" (String.concat ", " (loop_arg (Array.to_list f.assigns))));
		if reg_remap then begin
			for i=0 to nregs-1 do
				write (Printf.sprintf "\tr%-2d %-10s%s" i (tstr f.regs.(i)) (if ctx.r_reg_map.(i) < 0 then " unused" else if ctx.r_reg_map.(i) = i then "" else Printf.sprintf " r%-2d" ctx.r_reg_map.(i)))
			done;
		end;
		loop 0 ctx.r_root;
		write "";
		write "";
		(match dump with None -> () | Some ch -> IO.flush ch);
	end;

	let code = ref f.code in
	let regs = ref f.regs in
	let debug = ref f.debug in

	if ctx.r_nop_count > 0 || reg_remap then begin
		let new_pos = Array.make (Array.length f.code) 0 in
		let jumps = ref [] in
		let out_pos = ref 0 in
		let out_code = Array.make (Array.length f.code - ctx.r_nop_count) (ONop "") in
		let new_debug = Array.make (Array.length f.code - ctx.r_nop_count) (0,0,Globals.null_pos) in
		Array.iteri (fun i op ->
			Array.unsafe_set new_pos i !out_pos;
			match op with
			| ONop _ -> ()
			| _ ->
				(match op with
				| OJTrue _ | OJFalse _ | OJNull _ | OJNotNull _  | OJSLt _ | OJSGte _ | OJSGt _ | OJSLte _ | OJNotLt _ | OJNotGte _ | OJULt _ | OJUGte _ | OJEq _ | OJNotEq _ | OJAlways _ | OSwitch _  | OTrap _ ->
					jumps := i :: !jumps
				| _ -> ());
				let op = if reg_remap then opcode_map (fun r -> Array.unsafe_get reg_map r) (fun r -> Array.unsafe_get reg_map r) op else op in
				Array.unsafe_set out_code (!out_pos) op;
				Array.unsafe_set new_debug (!out_pos) (Array.unsafe_get f.debug i);
				incr out_pos
		) f.code;
		List.iter (fun j ->
			let pos d =
				Array.unsafe_get new_pos (j + 1 + d) - Array.unsafe_get new_pos (j + 1)
			in
			let p = new_pos.(j) in
			Array.unsafe_set out_code p (match Array.unsafe_get out_code p with
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
			| OJNotLt (a,b,d) -> OJNotLt (a,b,pos d)
			| OJNotGte (a,b,d) -> OJNotGte (a,b,pos d)
			| OJEq (a,b,d) -> OJEq (a,b,pos d)
			| OJNotEq (a,b,d) -> OJNotEq (a,b,pos d)
			| OJAlways d -> OJAlways (pos d)
			| OSwitch (r,cases,send) -> OSwitch (r, Array.map pos cases, pos send)
			| OTrap (r,d) -> OTrap (r,pos d)
			| _ -> Globals.die "" __LOC__)
		) !jumps;

		let assigns = !assigns in
		Array.iteri (fun idx (i,p) -> if p >= 0 then Array.unsafe_set assigns idx (i, Array.unsafe_get new_pos p)) assigns;

		code := out_code;
		debug := new_debug;
		if reg_remap then begin
			let new_regs = Array.make ctx.r_used_regs HVoid in
			for i=0 to nregs-1 do
				let p = Array.unsafe_get reg_map i in
				if p >= 0 then Array.unsafe_set new_regs p  (Array.unsafe_get f.regs i)
			done;
			regs := new_regs;
		end;
	end;
	{ f with code = !code; regs = !regs; debug = !debug; assigns = !assigns }

let _optimize (f:fundecl) =
	let nregs = Array.length f.regs in
	let op index = f.code.(index) in
	let set_op index op = f.code.(index) <- op in
	let nop_count = ref 0 in
	let set_nop index r = f.code.(index) <- (ONop r); incr nop_count in

	let blocks_pos, root = code_graph f in

	let read_counts = Array.make nregs 0 in
	let write_counts = Array.make nregs 0 in
	let last_write = Array.make nregs (-1) in

	let bit_regs = 30 in
	let stride = (nregs + bit_regs - 1) / bit_regs in
	let live_bits = Array.make (Array.length f.code * stride) 0 in

	let reg_moved = Hashtbl.create 0 in
	let add_reg_moved p w r =
		Hashtbl.add reg_moved p (r,last_write.(r))
	in

	let set_live r min max =
		let offset = r / bit_regs in
		let mask = 1 lsl (r - offset * bit_regs) in
		if min < 0 || max >= Array.length f.code then Globals.die "" __LOC__;
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

	let is_packed_field o fid =
		match f.regs.(o) with
		| HStruct p | HObj p ->
			let ft = (try snd (resolve_field p fid) with Not_found -> assert false) in
			(match ft with
			| HPacked _ -> true
			| _ -> false)
		| _ ->
			false
	in

(*
	let print_state i s =
		let state_str s =
			if s.ralias == s && s.rbind == [] then "" else
			Printf.sprintf "%d%s[%s]" s.rindex (if s.ralias == s then "" else "=" ^ string_of_int s.ralias.rindex) (String.concat "," (List.map (fun s -> string_of_int s.rindex) s.rbind))
		in
		write (Printf.sprintf "@%X %s" i (String.concat " " (Array.to_list (Array.map state_str s))))
	in
*)

	let rec propagate b =
		let state = if b.bloop then
			empty_state()
		else match b.bprev with
		| [] -> empty_state()
		| b2 :: l ->
			let s = get_state b2 in
			let s = (match b2.bnext with
			| [] -> Globals.die "" __LOC__
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
			(* print_state i state; (* debug *) *)
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
			| OMov (d, v) | OToInt (d, v) | OToSFloat (d,v) when d = v ->
				add_reg_moved i d v;
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
			| ONew r ->
				let s = state.(r) in
				do_write r;
				s.rnullcheck <- true;
			| OToVirtual (v,o) ->
				do_read o;
				do_write v;
				state.(v).rnullcheck <- state.(o).rnullcheck
			| OField (r,o,fid) when (match f.regs.(r) with HStruct _ -> true | _ -> false) ->
				do_read o;
				do_write r;
				if is_packed_field o fid then state.(r).rnullcheck <- true;
			| OGetThis (r,fid) when (match f.regs.(r) with HStruct _ -> true | _ -> false) ->
				do_write r;
				if is_packed_field 0 fid then state.(r).rnullcheck <- true;
			| OGetArray (r,arr,idx) ->
				do_read arr;
				do_read idx;
				do_write r;
				(match f.regs.(arr) with HAbstract _ -> state.(r).rnullcheck <- true | _ -> ());
			| _ ->
				opcode_fx (fun r read ->
					if read then do_read r else do_write r
				) op
			);
			loop (i + 1)
		in
		loop b.bstart;
		b.bstate <- Some state;
		List.iter (fun b2 -> ignore (get_state b2)) b.bnext

	and get_state b =
		match b.bstate with
		| None ->
			(* recurse before calculating *)
			List.iter (fun b2 -> if b2.bstart < b.bstart then ignore(get_state b2)) b.bprev;
			(match b.bstate with
			| None ->
				propagate b;
				get_state b
			| Some b -> b);
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
			) ISet.empty in
			let need_sub bl = ISet.filter (fun r ->
				try
					let w = PMap.find r b.bwrite in
					set_live r (w + 1) b.bend;
					false
				with Not_found ->
					set_live r b.bstart b.bend;
					true
			) (need_sub bl) in
			let need = ISet.union b.bneed (need_sub b.bnext) in
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
				(* do-while loop : recompute self after recompute all next *)
				let need = ISet.union b.bneed (need_sub b.bnext) in
				b.bneed_all <- Some need;
			end;
			Option.get b.bneed_all
	in
	ignore(live root);

	(* nop *)

	for i=0 to Array.length f.code - 1 do
		(match op i with
		| OMov (d,r) when not (is_live d (i + 1)) ->
			let n = read_counts.(r) - 1 in
			read_counts.(r) <- n;
			write_counts.(d) <- write_counts.(d) - 1;
			add_reg_moved i d r;
			set_nop i "unused"
		| OJAlways d when d >= 0 ->
			let rec loop k =
				if k = d then set_nop i "nojmp" else
				match f.code.(i + k + 1) with
				| ONop _ -> loop (k + 1)
				| _ -> ()
			in
			loop 0
		| _ -> ());
	done;

	(* reg map *)

	let used_regs = ref 0 in
	let reg_map = read_counts in
	let nargs = (match f.ftype with HFun (args,_) -> List.length args | _ -> Globals.die "" __LOC__) in
	for i=0 to nregs-1 do
		if read_counts.(i) > 0 || write_counts.(i) > 0 || i < nargs then begin
			reg_map.(i) <- !used_regs;
			incr used_regs;
		end else
			reg_map.(i) <- -1;
	done;
	{
		r_root = root;
		r_blocks_pos = blocks_pos;
		r_nop_count = !nop_count;
		r_used_regs = !used_regs;
		r_live_bits = live_bits;
		r_reg_map = reg_map;
		r_reg_moved = reg_moved;
	}

type cache_elt = {
	c_code : opcode array;
	c_rctx : rctx;
	c_remap_indexes : int array;
	mutable c_last_used : int;
}

let opt_cache = ref PMap.empty
let used_mark = ref 0

let optimize dump get_str (f:fundecl) (hxf:Type.tfunc) =
	let old_code = match dump with None -> f.code | Some _ -> Array.copy f.code in
	try
		let c = PMap.find hxf (!opt_cache) in
		c.c_last_used <- !used_mark;
		if Array.length f.code <> Array.length c.c_code then Globals.die "" __LOC__;
		let code = c.c_code in
		Array.iter (fun i ->
			let op = (match Array.unsafe_get code i, Array.unsafe_get f.code i with
			| OInt (r,_), OInt (_,idx) -> OInt (r,idx)
			| OFloat (r,_), OFloat (_,idx) -> OFloat (r,idx)
			| OBytes (r,_), OBytes (_,idx) -> OBytes (r,idx)
			| OString (r,_), OString (_,idx) -> OString (r,idx)
			| OCall0 (r,_), OCall0 (_,idx) -> OCall0 (r,idx)
			| OCall1 (r,_,a), OCall1 (_,idx,_) -> OCall1 (r,idx,a)
			| OCall2 (r,_,a,b), OCall2 (_,idx,_,_) -> OCall2 (r,idx,a,b)
			| OCall3 (r,_,a,b,c), OCall3 (_,idx,_,_,_) -> OCall3 (r,idx,a,b,c)
			| OCall4 (r,_,a,b,c,d), OCall4 (_,idx,_,_,_,_) -> OCall4 (r,idx,a,b,c,d)
			| OCallN (r,_,pl), OCallN (_,idx,_) -> OCallN (r,idx,pl)
			| OStaticClosure (r,_), OStaticClosure (_,idx) -> OStaticClosure (r,idx)
			| OInstanceClosure (r,_,v), OInstanceClosure (_,idx,_) -> OInstanceClosure (r,idx,v)
			| OGetGlobal (r,_), OGetGlobal (_,g) -> OGetGlobal (r,g)
			| OSetGlobal (_,v), OSetGlobal (g,_) -> OSetGlobal (g,v)
			| ODynGet (r,o,_), ODynGet (_,_,idx) -> ODynGet (r,o,idx)
			| ODynSet (o,_,v), ODynSet (_,idx,_) -> ODynSet (o,idx,v)
			| OType (r,_), OType (_,t) -> OType (r,t)
			| _ -> Globals.die "" __LOC__) in
			Array.unsafe_set code i op
		) c.c_remap_indexes;
		remap_fun c.c_rctx { f with code = code } dump get_str old_code
	with Not_found ->
		let rctx = _optimize f in
		let old_ops = f.code in
		let fopt = remap_fun rctx f dump get_str old_code in
		Hashtbl.iter (fun _ b ->
			b.bstate <- None;
			if dump = None then begin
				b.bneed <- ISet.empty;
				b.bneed_all <- None;
			end;
		) rctx.r_blocks_pos;
		let idxs = DynArray.create() in
		Array.iteri (fun i op ->
			match op with
			| OInt _ | OFloat _ | OBytes _ | OString _
			| OCall0 _ | OCall1 _ | OCall2 _ | OCall3 _ | OCall4 _ | OCallN _ | OStaticClosure _
			| OInstanceClosure _ | OGetGlobal _	| OSetGlobal _ | ODynGet _ | ODynSet _	| OType _ ->
				DynArray.add idxs i
			| _ -> ()
		) old_ops;
		(*opt_cache := PMap.add hxf {
			c_code = old_ops;
			c_rctx = rctx;
			c_last_used = !used_mark;
			c_remap_indexes = DynArray.to_array idxs;
		} (!opt_cache);*)
		fopt

let clean_cache() =
	PMap.iter (fun k c ->
		if !used_mark - c.c_last_used > 3 then opt_cache := PMap.remove k !opt_cache;
	) (!opt_cache);
	incr used_mark
