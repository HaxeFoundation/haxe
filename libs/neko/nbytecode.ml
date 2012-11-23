(*
 *  Neko Compiler
 *  Copyright (c)2005 Motion-Twin
 *
 *  This library is free software; you can redistribute it and/lor
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, lor (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY lor FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License lor the LICENSE file for more details.
 *)

type opcode =
	(* getters *)
	| AccNull
	| AccTrue
	| AccFalse
	| AccThis
	| AccInt of int
	| AccStack of int
	| AccGlobal of int
	| AccEnv of int
	| AccField of string
	| AccArray
	| AccIndex of int
	| AccBuiltin of string
	(* setters *)
	| SetStack of int
	| SetGlobal of int
	| SetEnv of int
	| SetField of string
	| SetArray
	| SetIndex of int
	| SetThis
	(* stack ops *)
	| Push
	| Pop of int
	| Call of int
	| ObjCall of int
	| Jump of int
	| JumpIf of int
	| JumpIfNot of int
	| Trap of int
	| EndTrap
	| Ret of int
	| MakeEnv of int
	| MakeArray of int
	(* value ops *)
	| Bool
	| IsNull
	| IsNotNull
	| Add
	| Sub
	| Mult
	| Div
	| Mod
	| Shl
	| Shr
	| UShr
	| Or
	| And
	| Xor
	| Eq
	| Neq
	| Gt
	| Gte
	| Lt
	| Lte
	| Not
	(* extra ops *)
	| TypeOf
	| Compare
	| Hash
	| New
	| JumpTable of int
	| Apply of int
	| AccStack0
	| AccStack1
	| AccIndex0
	| AccIndex1
	| PhysCompare
	| TailCall of int * int
	| Loop
	(* ocaml-specific *)
	| AccInt32 of int32

type global =
	| GlobalVar of string
	| GlobalFunction of int * int
	| GlobalString of string
	| GlobalFloat of string
	| GlobalDebug of string array * ((int * int) array)
	| GlobalVersion of int

exception Invalid_file

let error msg = failwith msg

let trap_stack_delta = 6

let hash_field f =
	let h = ref 0 in
	for i = 0 to String.length f - 1 do
		h := !h * 223 + int_of_char (String.unsafe_get f i);
	done;
	if Sys.word_size = 64 then Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int !h) 1) 1) else !h


let op_param x =
	match x with
	| AccInt _
	| AccInt32 _
	| AccStack _
	| AccGlobal _
	| AccEnv _
	| AccField _
	| AccBuiltin _
	| SetStack _
	| SetGlobal _
	| SetEnv _
	| SetField _
	| Pop _
	| Call _
	| ObjCall _
	| Jump _
	| JumpIf _
	| JumpIfNot _
	| JumpTable _
	| Trap _
	| MakeEnv _
	| MakeArray _
	| Ret _
	| AccIndex _
	| SetIndex _
	| Apply _
	| TailCall _
		-> true
	| AccNull
	| AccTrue
	| AccFalse
	| AccThis
	| AccArray
	| SetArray
	| SetThis
	| Push
	| EndTrap
	| Bool
	| Add
	| Sub
	| Mult
	| Div
	| Mod
	| Shl
	| Shr
	| UShr
	| Or
	| And
	| Xor
	| Eq
	| Neq
	| Gt
	| Gte
	| Lt
	| Lte
	| IsNull
	| IsNotNull
	| Not
	| TypeOf
	| Compare
	| Hash
	| New
	| AccStack0
	| AccStack1
	| AccIndex0
	| AccIndex1
	| PhysCompare
	| Loop
		-> false

let code_tables ops =
	let ids = Hashtbl.create 0 in
	let fids = DynArray.create() in
	Array.iter (fun x ->
		match x with
		| AccField s
		| SetField s
		| AccBuiltin s ->
			let id = hash_field s in
			(try
				let f = Hashtbl.find ids id in
				if f <> s then error("Field hashing conflict " ^ s ^ " and " ^ f);
			with Not_found ->
				Hashtbl.add ids id s;
				DynArray.add fids s
			)
		| _ -> ()
	) ops;
	let p = ref 0 in
	let pos = Array.make (Array.length(ops) + 1) 0 in
	Array.iteri (fun i op ->
		pos.(i) <- !p;
		p := !p + (if op_param op then 2 else 1);
	) ops;
	pos.(Array.length ops) <- !p;
	(DynArray.to_array fids , pos , !p)

let write_debug_infos ch files inf =
	let nfiles = Array.length files in
	(*
	// the encoding of nfiles was set to keep
	// backward compatibility with 1.3 which
	// only allowed up to 127 filenames
	*)
	let lot_of_files = ref false in
	if nfiles < 0x80 then
		IO.write_byte ch nfiles
	else if nfiles < 0x8000 then begin
		lot_of_files := true;
		IO.write_byte ch ((nfiles lsr 8) lor 0x80);
		IO.write_byte ch (nfiles land 0xFF);
	end else
		assert false;
	Array.iter (fun s -> IO.write_string ch s) files;
    IO.write_i32 ch (Array.length inf);
	let curfile = ref 0 in
	let curpos = ref 0 in
	let rcount = ref 0 in
	let rec flush_repeat p =
		if !rcount > 0 then begin
			if !rcount > 15 then begin
				IO.write_byte ch ((15 lsl 2) lor 2);
				rcount := !rcount - 15;
				flush_repeat(p)
			end else begin
				let delta = p - !curpos in
				let delta = (if delta > 0 && delta < 4 then delta else 0) in
				IO.write_byte ch ((delta lsl 6) lor (!rcount lsl 2) lor 2);
				rcount := 0;
				curpos := !curpos + delta;
			end
		end
	in
	Array.iter (fun (f,p) ->
		if f <> !curfile then begin
			flush_repeat(p);
			curfile := f;
			if !lot_of_files then begin
				IO.write_byte ch ((f lsr 7) lor 1);
				IO.write_byte ch (f land 0xFF);
			end else
				IO.write_byte ch ((f lsl 1) lor 1);
		end;
		if p <> !curpos then flush_repeat(p);
		if p = !curpos then
			rcount := !rcount + 1
		else
			let delta = p - !curpos in
			if delta > 0 && delta < 32 then
				IO.write_byte ch ((delta lsl 3) lor 4)
			else begin
				IO.write_byte ch (p lsl 3);
				IO.write_byte ch (p lsr 5);
				IO.write_byte ch (p lsr 13);
			end;
			curpos := p;
	) inf;
	flush_repeat(!curpos)

let write ch (globals,ops) =
	IO.nwrite ch "NEKO";
	let ids , pos , csize = code_tables ops in
	IO.write_i32 ch (Array.length globals);
	IO.write_i32 ch (Array.length ids);
	IO.write_i32 ch csize;
	Array.iter (fun x ->
		match x with
		| GlobalVar s -> IO.write_byte ch 1; IO.write_string ch s
		| GlobalFunction (p,nargs) -> IO.write_byte ch 2; IO.write_i32 ch (pos.(p) lor (nargs lsl 24))
		| GlobalString s -> IO.write_byte ch 3; IO.write_ui16 ch (String.length s); IO.nwrite ch s
		| GlobalFloat s -> IO.write_byte ch 4; IO.write_string ch s
		| GlobalDebug (files,inf) -> IO.write_byte ch 5; write_debug_infos ch files inf;
		| GlobalVersion v -> IO.write_byte ch 6; IO.write_byte ch v
	) globals;
	Array.iter (fun s ->
		IO.write_string ch s;
	) ids;
	Array.iteri (fun i op ->
		let pop = ref None in
		let opid = (match op with
			| AccNull -> 0
			| AccTrue -> 1
			| AccFalse -> 2
			| AccThis -> 3
			| AccInt n -> pop := Some n; 4
			| AccInt32 n ->
				let opid = 4 in
				IO.write_byte ch ((opid lsl 2) lor 3);
				IO.write_real_i32 ch n;
				-1
			| AccStack n -> pop := Some (n - 2); 5
			| AccGlobal n -> pop := Some n; 6
			| AccEnv n -> pop := Some n; 7
			| AccField s -> pop := Some (hash_field s); 8
			| AccArray -> 9
			| AccIndex n -> pop := Some (n - 2); 10
			| AccBuiltin s -> pop := Some (hash_field s); 11
			| SetStack n -> pop := Some n; 12
			| SetGlobal n -> pop := Some n; 13
			| SetEnv n -> pop := Some n; 14
			| SetField s -> pop := Some (hash_field s); 15
			| SetArray -> 16
			| SetIndex n -> pop := Some n; 17
			| SetThis -> 18
			| Push -> 19
			| Pop n -> pop := Some n; 20
			| Call n -> pop := Some n; 21
			| ObjCall n -> pop := Some n; 22
			| Jump n -> pop := Some (pos.(i+n) - pos.(i)); 23
			| JumpIf n -> pop := Some (pos.(i+n) - pos.(i)); 24
			| JumpIfNot n -> pop := Some (pos.(i+n) - pos.(i)); 25
			| Trap n -> pop := Some (pos.(i+n) - pos.(i)); 26
			| EndTrap -> 27
			| Ret n -> pop := Some n; 28
			| MakeEnv n -> pop := Some n; 29
			| MakeArray n -> pop := Some n; 30
			| Bool -> 31
			| IsNull -> 32
			| IsNotNull -> 33
			| Add -> 34
			| Sub -> 35
			| Mult -> 36
			| Div -> 37
			| Mod -> 38
			| Shl -> 39
			| Shr -> 40
			| UShr -> 41
			| Or -> 42
			| And -> 43
			| Xor -> 44
			| Eq -> 45
			| Neq -> 46
			| Gt -> 47
			| Gte -> 48
			| Lt -> 49
			| Lte -> 50
			| Not -> 51
			| TypeOf -> 52
			| Compare -> 53
			| Hash -> 54
			| New -> 55
			| JumpTable n -> pop := Some n; 56
			| Apply n -> pop := Some n; 57
			| AccStack0 -> 58
			| AccStack1 -> 59
			| AccIndex0 -> 60
			| AccIndex1 -> 61
			| PhysCompare -> 62
			| TailCall (args,st) -> pop := Some (args lor (st lsl 3)); 63
			| Loop -> pop := Some 64; 0
		) in
		match !pop with
		| None ->
			if opid >= 0 then IO.write_byte ch (opid lsl 2)
		| Some n ->
			if opid < 32 && (n = 0 || n = 1) then
				IO.write_byte ch ((opid lsl 3) lor (n lsl 2) lor 1)
			else if n >= 0 && n <= 0xFF then begin
				IO.write_byte ch ((opid lsl 2) lor 2);
				IO.write_byte ch n;
			end else begin
				IO.write_byte ch ((opid lsl 2) lor 3);
				IO.write_i32 ch n;
			end
	) ops
