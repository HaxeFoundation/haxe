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

type block = {
	bstart : int;
	mutable bend : int;
	mutable bnext : block list;
	mutable bprev : block list;
	mutable bloop : bool;
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
	| OTrap _ ->
		()
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
	| ODump r ->
		read r

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
	let op index = f.code.(index) in
	let write str = match dump with None -> () | Some ch -> IO.nwrite ch (str ^ "\n") in

	let blocks_pos, root = code_graph f in
	
	(* build registers liveness *)
(*	
	let rec liveness (b:block) regs =
		let regs = ref regs in
		let rec loop i =
			if i > b.bend then ()
			else begin
				opcode_fx (fun r read ->
					let ranges = (try PMap.find r !regs with Not_found -> []) in
					let ranges = if read then 
						(match ranges with [] -> [(i,b.bstart)] | (_,write) :: l -> (i,write) :: l) 
					else
						(-1,i) :: ranges
					in
					regs := PMap.add r ranges !regs;
				) (op i);
				loop (i + 1)
			end;
		in
		loop b.bstart;
		let start = !regs in
		if b.bloop then begin
			start;
		end else
			List.fold_left (fun regs b2 ->
				let regs2 = liveness b2 start in
				(* todo : regs union regs2 *)
				regs2
			) start b.bnext 
	in
	let rec loop i args map =
		match args with
		| [] -> map
		| _ :: args ->
			loop (i + 1) args (PMap.add i [(-1,-1)] map) 
	in
	let live = liveness root (loop 0 (match f.ftype with HFun (args,_) -> args | _ -> assert false) PMap.empty) in
	*)
	(* done *)
	
	if dump <> None then begin
		let rec loop i block =
			if i = Array.length f.code then () else
			let block = try 
				let nblock = Hashtbl.find blocks_pos i in
				write (Printf.sprintf "\t----- [%s] (%d)"
					(String.concat "," (List.map (fun b -> string_of_int b.bstart) nblock.bnext))
					nblock.bend
				);
				nblock
			with Not_found ->
				block
			in
			write (Printf.sprintf "\t@%d %s" i (ostr string_of_int (op i)));
			loop (i + 1) block
		in
		write (fundecl_name f);
		loop 0 root;
		write "";
		write "";
	end;

	f