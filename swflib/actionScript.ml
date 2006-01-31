(*
 *  This file is part of SwfLib
 *  Copyright (c)2004 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Swf
open IO
open Printf

let push_item_length = function
	| PString s -> String.length s + 1
	| PFloat _ -> 4
	| PNull -> 0
	| PUndefined -> 0
	| PReg _ -> 1
	| PBool _ -> 1
	| PDouble _ -> 8
	| PInt _ -> 4
	| PStack _ -> 1
	| PStack2 _ -> 2

let push_item_id = function
	| PString s -> 0
	| PFloat _ -> 1
	| PNull -> 2
	| PUndefined -> 3
	| PReg _ -> 4
	| PBool _ -> 5
	| PDouble _ -> 6
	| PInt _ -> 7
	| PStack _ -> 8
	| PStack2 _ -> 9


let opcodes = Hashtbl.create 0
let opcodes_rev = Hashtbl.create 0
let opcodes_names = Hashtbl.create 0

let ( => ) code (op,name) =
	Hashtbl.add opcodes op code;
	Hashtbl.add opcodes_rev code op;
	Hashtbl.add opcodes_names op name

let short_op_codes = begin
	0x00 => (AEnd,"END");
	0x04 => (ANextFrame,"NEXTFRAME");
	0x05 => (APrevFrame,"PREVFRAME");
	0x06 => (APlay,"PLAY");
	0x07 => (AStop,"STOP");
	0x08 => (AToggleHighQuality,"TGLHIGHQULTY");
	0x09 => (AStopSounds,"STOPSOUNDS");
	0x0A => (AAddNum,"ADDNUM");
	0x0B => (ASubtract,"SUB");
	0x0C => (AMultiply,"MULT");
	0x0D => (ADivide,"DIV");
	0x0E => (ACompareNum,"CMP");
	0x0F => (AEqualNum,"EQNUM");
	0x10 => (ALogicalAnd,"LAND");
	0x11 => (ALogicalOr,"LOR");
	0x12 => (ANot,"NOT");
	0x13 => (AStringEqual,"STREQ");
	0x14 => (AStringLength,"STRLEN");
	0x15 => (ASubString,"SUBSTR");
	0x17 => (APop,"POP");
	0x18 => (AToInt,"TOINT");
	0x1C => (AEval,"EVAL");
	0x1D => (ASet,"SET");
	0x20 => (ATellTarget,"TELLTARGET");
	0x21 => (AStringAdd,"STRADD");
	0x22 => (AGetProperty,"GETPROP");
	0x23 => (ASetProperty,"SETPROP");
	0x24 => (ADuplicateMC,"DUPLICATEMC");
	0x25 => (ARemoveMC,"REMOVEMC");
	0x26 => (ATrace,"TRACE");
	0x27 => (AStartDrag,"STARTDRAG");
	0x28 => (AStopDrag,"STOPDRAG");
	0x2A => (AThrow,"THROW");
	0x2B => (ACast,"CAST");
	0x2C => (AImplements,"IMPLEMENTS");
	0x2D => (AFSCommand2,"FSCOMMAND2");
	0x30 => (ARandom,"RANDOM");
	0x31 => (AMBStringLength,"MBSTRLEN");
	0x32 => (AOrd,"ORD");
	0x33 => (AChr,"CHR");
	0x34 => (AGetTimer,"GETTIMER");
	0x35 => (AMBStringSub,"MBSTRSUB");
	0x36 => (AMBOrd,"MBORD");
	0x37 => (AMBChr,"MBCHR");
	0x3A => (ADeleteObj,"DELETEOBJ");
	0x3B => (ADelete,"DELETE");
	0x3C => (ALocalAssign,"VARSET");
	0x3D => (ACall,"CALL");
	0x3E => (AReturn,"RET");
	0x3F => (AMod,"MOD");
	0x40 => (ANew,"NEW");
	0x41 => (ALocalVar,"VAR");
	0x42 => (AInitArray,"ARRAY");
	0x43 => (AObject,"OBJECT");
	0x44 => (ATypeOf,"TYPEOF");
	0x45 => (ATargetPath,"TARGETPATH");
	0x46 => (AEnum,"ENUM");
	0x47 => (AAdd,"ADD");
	0x48 => (ACompare,"CMP");
	0x49 => (AEqual,"EQ");
	0x4A => (AToNumber,"TONUMBER");
	0x4B => (AToString,"TOSTRING");
	0x4C => (ADup,"DUP");
	0x4D => (ASwap,"SWAP");
	0x4E => (AObjGet,"OBJGET");
	0x4F => (AObjSet,"OBJSET");
	0x50 => (AIncrement,"INCR");
	0x51 => (ADecrement,"DECR");
	0x52 => (AObjCall,"OBJCALL");
	0x53 => (ANewMethod,"NEWMETHOD");
	0x54 => (AInstanceOf,"INSTANCEOF");
	0x55 => (AEnum2,"ENUM2");
	0x60 => (AAnd,"AND");
	0x61 => (AOr,"OR");
	0x62 => (AXor,"XOR");
	0x63 => (AShl,"SHL");
	0x64 => (AShr,"SHR");
	0x65 => (AAsr,"ASR");
	0x66 => (APhysEqual,"PHYSEQ");
	0x67 => (AGreater,"GT");
	0x68 => (AStringGreater,"STRGT");
	0x69 => (AExtends,"EXTENDS");
	0x9E => (ACallFrame,"CALLFRAME"); (* special case *)

end

let action_id = function
	| AGotoFrame _ -> 0x81
	| AGetURL _ -> 0x83
	| ASetReg _ -> 0x87
	| AStringPool _ -> 0x88
	| AWaitForFrame _ -> 0x8A
	| ASetTarget _ -> 0x8B
	| AGotoLabel _ -> 0x8C
	| AWaitForFrame2 _ -> 0x8D
	| AFunction2 _ -> 0x8E
	| ATry _ -> 0x8F
	| AWith _ -> 0x94
	| APush _ -> 0x96
	| AJump _ -> 0x99
	| AGetURL2 _ -> 0x9A
	| AFunction _ -> 0x9B
	| ACondJump _ -> 0x9D
	| AGotoFrame2 _ -> 0x9F
	| AUnknown (id,_) -> id

	| op ->
		try
			Hashtbl.find opcodes op
		with
			Not_found -> error "Unknown opcode id"

let action_data_length = function
	| AGotoFrame _ ->
		2
	| AGetURL (url,target) ->
		2 + String.length url + String.length target
	| ASetReg _ ->
		1
	| AStringPool strs ->
		List.fold_left (fun acc item -> acc + 1 + String.length item) 2 strs
	| AWaitForFrame _ ->
		3
	| AFunction2 f ->
		let base = String.length f.f2_name + 1 + 2 + 1 + 2 + 2 in
		List.fold_left (fun acc (_,s) -> acc + 2 + String.length s) base f.f2_args
	| ASetTarget target ->
		String.length target + 1
	| AGotoLabel label ->
		String.length label + 1
	| AWaitForFrame2 _ ->
		1
	| ATry t ->
		1 + 6 + (match t.tr_style with TryVariable n -> String.length n + 1 | TryRegister _ -> 1)
	| AWith _ ->
		2 (* the string does not count in length *)
	| APush items ->
		List.fold_left (fun acc item -> acc + 1 + push_item_length item) 0 items
	| AJump _ ->
		2
	| AGetURL2 _ ->
		1
	| AFunction f ->
		List.fold_left (fun acc s -> acc + 1 + String.length s) 4 (f.f_name :: f.f_args)
	| ACondJump _ ->
		2
	| AGotoFrame2 (_,id) ->
		1 + (if id = None then 0 else 2)
	| AUnknown (_,data) ->
		String.length data
	| _ ->
		0

let action_length a = 
	let len = (if action_id a >= 0x80 then 3 else 1) in
	len + action_data_length a

let actions_length acts =
	DynArray.fold_left (fun acc a -> acc + action_length a) (action_length AEnd) acts

let read_mm_double ch =
	let i1 = Int64.of_int32 (read_real_i32 ch) in
	let i2 = Int64.of_int32 (read_real_i32 ch) in
	let i2 = (if i2 < Int64.zero then Int64.add i2 (Int64.shift_left Int64.one 32) else i2) in
	Int64.float_of_bits (Int64.logor i2 (Int64.shift_left i1 32))

let write_mm_double ch f =
	let i64 = Int64.bits_of_float f in
	write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical i64 32));
	write_real_i32 ch (Int64.to_int32 i64)	

let read_string_max ch len =
	let b = Buffer.create 0 in
	let rec loop l =
		if l = 0 then begin
			let s = Buffer.contents b in
			String.sub s 0 (String.length s - 1)
		end else
			let c = read ch in
			if c = '\000' then 
				Buffer.contents b
			else begin
				Buffer.add_char b c;
				loop (l - 1)
			end;
	in
	loop len

let parse_push_item ch len = 
	let id = read_byte ch in
	match id with
	| 0 -> PString (read_string_max ch len)
	| 1 -> PFloat (read_real_i32 ch)
	| 2 -> PNull
	| 3 -> PUndefined
	| 4 -> PReg (read_byte ch)
	| 5 -> PBool (read_byte ch <> 0)
	| 6 -> PDouble (read_mm_double ch)
	| 7 -> PInt (read_real_i32 ch)
	| 8 -> PStack (read_byte ch)
	| 9 -> PStack2 (read_ui16 ch)
	| _ -> error (sprintf "Unknown PUSH item id : %d" id)

let rec parse_push_items ch len =
	if len < 0 then error "PUSH parse overflow";
	if len = 0 then
		 []
	else
		let item = parse_push_item ch len in
		item :: parse_push_items ch (len - 1 - push_item_length item)

let rec read_strings ch n =
	if n = 0 then
		[]
	else
		let s = read_string ch in
		s :: read_strings ch (n-1)

let parse_function_decl ch =
	let name = read_string ch in
	let nargs = read_ui16 ch in
	let args = read_strings ch nargs in
	let clen = read_ui16 ch in
	{
		f_name = name;
		f_args = args;
		f_codelen = clen;
	}

let parse_f2_flags n =
	let flags = ref [] in
	let v = ref 1 in
	let add_flag f =
		if n land !v <> 0 then flags := f :: !flags;
		v := !v lsl 1
	in
	List.iter add_flag 
		[ThisRegister; ThisNoVar; ArgumentsRegister; ArgumentsNoVar; SuperRegister; 
		 SuperNoVar; RootRegister; ParentRegister; GlobalRegister];
	!flags

let parse_function_decl2 ch =
	let name = read_string ch in
	let nargs = read_ui16 ch in
	let nregs = read_byte ch in
	let flags = parse_f2_flags (read_ui16 ch) in
	let rec loop n = 
		if n = 0 then
			[]
		else
			let r = read_byte ch in
			let s = read_string ch in
			(r,s) :: loop (n-1)
	in
	let args = loop nargs in
	let clen = read_ui16 ch in
	{
		f2_name = name;
		f2_args = args;
		f2_flags = flags;
		f2_codelen = clen;
		f2_nregs = nregs;
	}


let parse_action ch =
	let id = read_byte ch in
	let len = (if id >= 0x80 then read_ui16 ch else 0) in
	let len = (if len = 0xFFFF then 0 else len) in
	let act = 
		(match id with
		| 0x81 ->
			AGotoFrame (read_ui16 ch)
		| 0x83 -> 
			let url = read_string ch in
			let target = read_string ch in
			AGetURL (url,target)
		| 0x87 ->
			ASetReg (read_byte ch)
		| 0x88 ->
			let nstrs = read_ui16 ch in
			AStringPool (read_strings ch nstrs)
		| 0x8A ->
			let frame = read_ui16 ch in
			let skip = read_byte ch in
			AWaitForFrame (frame,skip)
		| 0x8B ->
			ASetTarget (read_string ch)
		| 0x8C ->
			AGotoLabel (read_string ch)
		| 0x8D ->
			AWaitForFrame2 (read_byte ch)
		| 0x8E ->
			AFunction2 (parse_function_decl2 ch)
		| 0x8F ->
			let flags = read_byte ch in
			let tsize = read_ui16 ch in
			let csize = read_ui16 ch in
			let fsize = read_ui16 ch in
			let tstyle = (if flags land 4 == 0 then TryVariable (read_string ch) else TryRegister (read_byte ch)) in
			ATry {
				tr_style = tstyle;
				tr_trylen = tsize;
				tr_catchlen = (if flags land 1 == 0 then None else Some csize);
				tr_finallylen = (if flags land 2 == 0 then None else Some fsize);
			}
		| 0x94 ->
			let size = read_ui16 ch in
			AWith size
		| 0x96 ->
			APush (parse_push_items ch len)
		| 0x99 ->
			AJump (read_i16 ch)
		| 0x9A ->
			AGetURL2 (read_byte ch)
		| 0x9B ->
			AFunction (parse_function_decl ch)
		| 0x9D ->
			ACondJump (read_i16 ch)
		| 0x9E ->
			ACallFrame
		| 0x9F ->
			let flags = read_byte ch in
			let play = flags land 1 <> 0 in
			let delta = (if flags land 2 == 0 then None else Some (read_ui16 ch)) in
			AGotoFrame2 (play,delta)
		| _ ->
			try
				Hashtbl.find opcodes_rev id
			with
				Not_found ->
					printf "Unknown Action 0x%.2X (%d)\n" id len;
					AUnknown (id,nread ch len)
	) in
(*	let len2 = action_data_length act in
	if len <> len2 then error (sprintf "Datalen mismatch for action 0x%.2X (%d != %d)" id len len2);
*)	act

let size_to_jump_index acts curindex size =
	let delta = ref 0 in
	let size = ref size in	
	if !size >= 0 then begin
		while !size > 0 do
			incr delta;
			size := !size - action_length (DynArray.get acts (curindex + !delta));
			if !size < 0 then error "Unaligned code";
		done;		
	end else begin
		while !size < 0 do
			size := !size + action_length (DynArray.get acts (curindex + !delta));
			if !size > 0 then error "Unaligned code";
			decr delta;
		done;
	end;
	!delta

let parse_actions ch =
	let acts = DynArray.create() in
	let rec loop() =
		match parse_action ch with
		| AEnd -> ()
		| AUnknown (0xFF,"") -> 
			DynArray.add acts APlay;
			DynArray.add acts APlay;
			DynArray.add acts APlay;
			loop()
		| a ->
			DynArray.add acts a;
			loop();
	in
	loop();
	(* process jump indexes *)
	let process_jump curindex = function
		| AJump size ->
			let index = size_to_jump_index acts curindex size in
			DynArray.set acts curindex (AJump index)
		| ACondJump size ->
			let index = size_to_jump_index acts curindex size in
			DynArray.set acts curindex (ACondJump index)
		| AFunction f ->
			let index = size_to_jump_index acts curindex f.f_codelen in
			DynArray.set acts curindex (AFunction { f with f_codelen = index })
		| AFunction2 f ->
			let index = size_to_jump_index acts curindex f.f2_codelen in
			DynArray.set acts curindex (AFunction2 { f with f2_codelen = index })
		| AWith size ->
			let index = size_to_jump_index acts curindex size in
			DynArray.set acts curindex (AWith index)
		| ATry t ->
			let tindex = size_to_jump_index acts curindex t.tr_trylen in
			let cindex = (match t.tr_catchlen with None -> None | Some size -> Some (size_to_jump_index acts (curindex + tindex) size)) in
			let findex = (match t.tr_finallylen with None -> None | Some size -> Some (size_to_jump_index acts (curindex + tindex + (match cindex with None -> 0 | Some i -> i)) size)) in
			DynArray.set acts curindex (ATry { t with tr_trylen = tindex; tr_catchlen = cindex; tr_finallylen = findex })
		| _ ->
			()
	in
	DynArray.iteri process_jump acts;
	acts

let jump_index_to_size acts curindex target =
	let size = ref 0 in
	if target >= 0 then begin
		for i = 1 to target do
			size := !size + action_length (DynArray.get acts (curindex + i));
		done;
	end else begin
		for i = 0 downto target+1 do
			size := !size - action_length (DynArray.get acts (curindex + i));
		done;
	end;
	!size

let rec write_strings ch = function
	| [] -> ()
	| s :: l ->
		write_string ch s;
		write_strings ch l

let write_push_item_data ch = function
	| PString s -> write_string ch s
	| PFloat f -> write_real_i32 ch f
	| PNull -> ()
	| PUndefined -> ()
	| PReg r -> write_byte ch r
	| PBool b -> write_byte ch (if b then 1 else 0)
	| PDouble f -> write_mm_double ch f
	| PInt n -> write_real_i32 ch n
	| PStack index -> write_byte ch index
	| PStack2 index -> write_ui16 ch index

let f2_flags_value flags =
	let fval = function
		| ThisRegister -> 1
		| ThisNoVar -> 2 
		| ArgumentsRegister -> 4
		| ArgumentsNoVar -> 8
		| SuperRegister -> 16
		| SuperNoVar -> 32
		| RootRegister -> 64
		| ParentRegister -> 128
		| GlobalRegister -> 256
	in
	List.fold_left (fun n f -> n lor (fval f)) 0 flags	

let write_action_data acts curindex ch = function
	| AGotoFrame frame ->
		write_ui16 ch frame
	| AGetURL (url,target) ->
		write_string ch url;
		write_string ch target
	| ASetReg reg ->
		write_byte ch reg
	| AStringPool strs ->
		write_ui16 ch (List.length strs);
		write_strings ch strs
	| AWaitForFrame (frame,skip) ->
		write_ui16 ch frame;
		write_byte ch skip
	| ASetTarget target ->
		write_string ch target
	| AGotoLabel label ->
		write_string ch label
	| AWaitForFrame2 n ->
		write_byte ch n
	| AFunction2 f ->
		write_string ch f.f2_name;
		write_ui16 ch (List.length f.f2_args);
		write_byte ch f.f2_nregs;
		write_ui16 ch (f2_flags_value f.f2_flags);
		List.iter (fun (r,s) ->
			write_byte ch r;
			write_string ch s;
		) f.f2_args;
		let size = jump_index_to_size acts curindex f.f2_codelen in
		write_ui16 ch size;
	| ATry t ->
		let tsize = jump_index_to_size acts curindex t.tr_trylen in
		let csize = (match t.tr_catchlen with None -> 0 | Some idx -> jump_index_to_size acts (curindex + t.tr_trylen) idx) in
		let fsize = (match t.tr_finallylen with None -> 0 | Some idx -> jump_index_to_size acts (curindex + t.tr_trylen + (match t.tr_catchlen with None -> 0 | Some n -> n)) idx) in		
		let flags = (if t.tr_catchlen <> None then 1 else 0) lor (if t.tr_finallylen <> None then 2 else 0) lor (match t.tr_style with TryRegister _ -> 4 | TryVariable _ -> 0) in
		write_byte ch flags;
		write_ui16 ch tsize;
		write_ui16 ch csize;
		write_ui16 ch fsize;
		(match t.tr_style with
		| TryVariable v -> write_string ch v
		| TryRegister r -> write_byte ch r)
	| AWith target ->
		let size = jump_index_to_size acts curindex target in
		write_ui16 ch size		
	| APush items ->
		List.iter (fun item ->
			write_byte ch (push_item_id item);
			write_push_item_data ch item
		) items
	| AJump target ->
		let size = jump_index_to_size acts curindex target in
		write_i16 ch size
	| AGetURL2 n ->
		write_byte ch n
	| AFunction f ->
		write_string ch f.f_name;
		write_ui16 ch (List.length f.f_args);
		write_strings ch f.f_args;
		let size = jump_index_to_size acts curindex f.f_codelen in
		write_ui16 ch size;
	| ACondJump target ->
		let size = jump_index_to_size acts curindex target in
		write_i16 ch size;
	| AGotoFrame2 (play,None) ->
		write_byte ch (if play then 1 else 0)
	| AGotoFrame2 (play,Some delta) ->
		write_byte ch (if play then 3 else 2);
		write_ui16 ch delta;
	| ACallFrame ->
		()
	| AUnknown (_,data) ->
		nwrite ch data
	| _ ->
		assert false

let write_action acts curindex ch a =
	let id = action_id a in
	let len = action_data_length a in
	if id < 0x80 && len > 0 then error "Invalid Action Written";
	write_byte ch id;
	if len > 0 || id >= 0x80 then begin
		write_ui16 ch len;
		write_action_data acts curindex ch a;
	end

let write_actions ch acts =
	DynArray.iteri (fun index act -> write_action acts index ch act) acts;
	write_action acts (DynArray.length acts) ch AEnd

let sprintf = Printf.sprintf

let action_string get_ident pos = function
	| AGotoFrame n -> sprintf "GOTOFRAME %d" n
	| AGetURL (a,b) -> sprintf "GETURL '%s' '%s'" a b
	| ASetReg n -> sprintf "SETREG %d" n
	| AStringPool strlist -> 
		let b = Buffer.create 0 in
		Buffer.add_string b "STRINGS ";
		let p = ref 0 in
		List.iter (fun s ->
			Buffer.add_string b (string_of_int !p);
			incr p;
			Buffer.add_char b ':';
			Buffer.add_string b s;
			Buffer.add_char b ' ';
		) strlist;
		Buffer.contents b
	| AWaitForFrame (i,j) -> sprintf "WAITFORFRAME %d %d" i j
	| ASetTarget s -> sprintf "SETTARGET %s" s
	| AGotoLabel s -> sprintf "GOTOLABEL %s" s
	| AWaitForFrame2 n -> sprintf "WAITFORFRAME2 %d" n
	| AFunction2 f ->
		let b = Buffer.create 0 in
		Buffer.add_string b "FUNCTION2 ";
		Buffer.add_string b f.f2_name;
		Buffer.add_char b '(';
		Buffer.add_string b (String.concat "," (List.map (fun (n,str) -> sprintf "%d:%s" n str) f.f2_args));
		Buffer.add_char b ')';
		Buffer.add_string b (sprintf " nregs:%d flags:%d " f.f2_nregs (f2_flags_value f.f2_flags));
		Buffer.add_string b (sprintf "0x%.4X" (pos + 1 + f.f2_codelen));
		Buffer.contents b
	| APush pl ->
		let b = Buffer.create 0 in
		Buffer.add_string b "PUSH";
		List.iter (fun it ->
			Buffer.add_char b ' ';
			match it with
			| PString s -> 
				Buffer.add_char b '"';
				Buffer.add_string b s;
				Buffer.add_char b '"'
			| PFloat _ ->
				Buffer.add_string b "<float>"
			| PNull ->
				Buffer.add_string b "null"
			| PUndefined ->
				Buffer.add_string b "undefined"
			| PReg n ->
				Buffer.add_string b (sprintf "reg:%d" n)
			| PBool fl ->
				Buffer.add_string b (if fl then "true" else "false")
			| PDouble _ ->
				Buffer.add_string b "<double>"
			| PInt i ->
				Buffer.add_string b (Int32.to_string i)
			| PStack n
			| PStack2 n ->
				Buffer.add_char b '[';
				Buffer.add_string b (string_of_int n);
				Buffer.add_char b ':';
				Buffer.add_string b (get_ident n);
				Buffer.add_char b ']';
		) pl;
		Buffer.contents b
	| ATry _ -> sprintf "TRY"
	| AWith n -> sprintf "WITH %d" n
	| AJump n -> sprintf "JUMP 0x%.4X" (n + pos + 1)
	| AGetURL2 n -> sprintf "GETURL2 %d" n
	| AFunction f ->
		let b = Buffer.create 0 in
		Buffer.add_string b "FUNCTION ";
		Buffer.add_string b f.f_name;
		Buffer.add_char b '(';
		Buffer.add_string b (String.concat "," f.f_args);
		Buffer.add_char b ')';
		Buffer.add_string b (sprintf " 0x%.4X" (pos + 1 + f.f_codelen));
		Buffer.contents b
	| ACondJump n -> sprintf "CJMP 0x%.4X" (n + pos + 1)
	| AGotoFrame2 (b,None) -> sprintf "GOTOFRAME2 %b" b
	| AGotoFrame2 (b,Some i) -> sprintf "GOTOFRAME2 %b %d" b i
	| AUnknown (tag,_) -> sprintf "??? 0x%.2X" tag
	| op ->
		try
			Hashtbl.find opcodes_names op
		with
			Not_found -> assert false
