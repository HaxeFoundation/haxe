open Printf
open Swf
open Tools

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

let ( => ) code op =
	Hashtbl.add opcodes op code;
	Hashtbl.add opcodes_rev code op

let short_op_codes = begin
	0x00 => AEnd;
	0x04 => ANextFrame;
	0x05 => APrefFrame;
	0x06 => APlay;
	0x07 => AStop;
	0x08 => AToggleHighQuality;
	0x09 => AStopSounds;
	0x0A => AAdd;
	0x0B => ASubtract;
	0x0C => AMultiply;
	0x0D => ADivide;
	0x0E => ACompare;
	0x0F => ANumberEqual;
	0x10 => ALogicalAnd;
	0x11 => ALogicalOr;
	0x12 => ANot;
	0x13 => AStringEqual;
	0x14 => AStringLength;
	0x15 => ASubString;
	0x17 => APop;
	0x18 => AToInt;
	0x1C => AEval;
	0x1D => ASet;
	0x20 => ATellTarget;
	0x21 => AStringAdd;
	0x22 => AGetProperty;
	0x23 => ASetProperty;
	0x24 => ADuplicateMC;
	0x25 => ARemoveMC;
	0x26 => ATrace;
	0x27 => AStartDrag;
	0x28 => AStopDrag;
	0x2A => AThrow;
	0x2B => ACast;
	0x2C => AImplement;
	0x30 => ARandom;
	0x31 => AMBStringLength;
	0x32 => AOrd;
	0x33 => AChr;
	0x34 => AGetTimer;
	0x35 => AMBStringSub;
	0x36 => AMBOrd;
	0x37 => AMBChr;
	0x3A => ADeleteObj;
	0x3B => ADelete;
	0x3C => ALocalVar;
	0x3D => ACall;
	0x3E => AReturn;
	0x3F => AMod;
	0x40 => ANew;
	0x41 => ALocalAssign;
	0x42 => AInitArray;
	0x43 => AObject;
	0x44 => ATypeOf;
	0x45 => ATargetPath;
	0x46 => AEnum;
	0x47 => AAdd2;
	0x48 => ACompare2;
	0x49 => AEqual;
	0x4A => AToNumber;
	0x4B => AToString;
	0x4C => ADup;
	0x4D => ASwap;
	0x4E => AObjGet;
	0x4F => AObjSet;
	0x50 => AIncrement;
	0x51 => ADecrement;
	0x52 => AObjCall;
	0x53 => ANewMethod;
	0x54 => AInstanceOf;
	0x55 => AEnum2;
	0x60 => AAnd;
	0x61 => AOr;
	0x62 => AXor;
	0x63 => AShl;
	0x64 => AShr;
	0x65 => AAsr;
	0x66 => APhysEqual;
	0x67 => AGreater;
	0x68 => AStringGreater;
	0x69 => AExtends;
	0x0E => ACallFrame; (* special case *)

end

(*/*
X	OP(0x8E,"FUNCTION2"); // see Tag
X	OP(0x8F,"TRY")
*/*)

let action_id = function
	| AGotoFrame _ -> 0x81
	| AGetURL _ -> 0x83
	| APushReg _ -> 0x87
	| AStringPool _ -> 0x88
	| AWaitForFrame _ -> 0x8A
	| ASetTarget _ -> 0x8B
	| AGotoLabel _ -> 0x8C
	| AWaitForFrame2 _ -> 0x8D
	| AFunction2 _ -> 0x8E
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
	| APushReg _ ->
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
	| AWith (s,_) ->
		2 + String.length s + 1
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

let parse_push_item ch = 
	let id = read_byte ch in
	match id with
	| 0 -> PString (read_string ch)
	| 1 -> PFloat (read ch 4)
	| 2 -> PNull
	| 3 -> PUndefined
	| 4 -> PReg (read_byte ch)
	| 5 -> PBool (read_byte ch <> 0)
	| 6 -> PDouble (read ch 8)
	| 7 -> PInt (read ch 4)
	| 8 -> PStack (read_byte ch)
	| 9 -> PStack2 (read_ui16 ch)
	| _ -> error (sprintf "Unknown PUSH item id : %d" id)

let rec parse_push_items ch len =
	if len < 0 then error "PUSH parse overflow";
	if len = 0 then
		 []
	else
		let item = parse_push_item ch in
		item :: parse_push_items ch (len - 1 - push_item_length item)

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

let parse_function_decl2 ch =
	let name = read_string ch in
	let nargs = read_ui16 ch in
	let nregs = read_byte ch in
	let flags = read_ui16 ch in
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
	let act = 
		(match id with
		| 0x81 ->
			AGotoFrame (read_ui16 ch)
		| 0x83 -> 
			let url = read_string ch in
			let target = read_string ch in
			AGetURL (url,target)
		| 0x87 ->
			APushReg (read_byte ch)
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
		| 0x94 ->
			let size = read_ui16 ch in
			let eval = read_string ch in
			AWith (eval,size)
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
					AUnknown (id,read ch len)
	) in
	let len2 = action_data_length act in
	if len <> len2 then error (sprintf "Datalen mismatch for action 0x%.2X (%d != %d)" id len len2);
	act

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
		let a = parse_action ch in
		if a <> AEnd then begin
			DynArray.add acts a;
			loop();
		end;
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
		| AWith (eval,size) ->
			let index = size_to_jump_index acts curindex size in
			DynArray.set acts curindex (AWith (eval,index))
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

let write_push_item_data ch = function
	| PString s -> write_string ch s
	| PFloat data -> output_string ch data
	| PNull -> ()
	| PUndefined -> ()
	| PReg r -> write_byte ch r
	| PBool b -> write_byte ch (if b then 1 else 0)
	| PDouble data -> output_string ch data
	| PInt data -> output_string ch data
	| PStack index -> write_byte ch index
	| PStack2 index -> write_ui16 ch index

let write_action_data acts curindex ch = function
	| AGotoFrame frame ->
		write_ui16 ch frame
	| AGetURL (url,target) ->
		write_string ch url;
		write_string ch target
	| APushReg reg ->
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
		write_ui16 ch f.f2_flags;
		List.iter (fun (r,s) ->
			write_byte ch r;
			write_string ch s;
		) f.f2_args;
		let size = jump_index_to_size acts curindex f.f2_codelen in
		write_ui16 ch size;
	| AWith (eval,target) ->
		let size = jump_index_to_size acts curindex target in
		write_ui16 ch size;
		write_string ch eval;
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
		write_i16 ch size
	| AGotoFrame2 (play,None) ->
		write_byte ch (if play then 1 else 0)
	| AGotoFrame2 (play,Some delta) ->
		write_byte ch (if play then 3 else 2);
		write_ui16 ch delta;
	| ACallFrame ->
		()
	| AUnknown (_,data) ->
		output_string ch data
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
