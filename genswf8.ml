(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
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
open Type

type local_ctx = {
	reg : int;
	sp : int;
}

type context = {
	(* code *)
	opcodes : actions;
	mutable code_pos : int;
	mutable stack_size : int;
	mutable opt_push : bool;
	mutable ident_count : int;

	(* management *)
	idents : (string,int) Hashtbl.t;
	locals : (string,local_ctx) PMap.t;
	types : (module_path,string) Hashtbl.t;
}

(* -------------------------------------------------------------- *)
(* Bytecode Helpers *)

type kind = 
	| VarReg of int
	| VarStr
	| VarObj

type push_style =
	| VStr of string
	| VInt of int
	| VInt32 of int32
	| VFloat of float
	| VReg of int
	| VThis
	| VNull
	| VSuper

let stack_delta = function
	| APush l -> List.length l
	| ASetReg _ -> 0
	| AAdd | ADivide | ASubtract | AMultiply | AMod | AStringAdd -> -1
	| AAnd | AOr | AXor | AShl | AShr | AAsr -> -1
	| ACompare | AGreater -> -1
	| AEval | ANot | AJump _ | AToInt | AToNumber | AToString | ATry _ | ASwap -> 0
	| ACondJump _ -> -1
	| AEqual | APhysEqual -> -1
	| ANew -> -1 (** only if 0 params **)
	| AObject | AInitArray -> 0 (** calculated outside **)
	| ASet -> -2
	| APop -> -1
	| AFunction _ | AFunction2 _ -> 1	
	| ADup -> 1
	| AWith _ -> -1
	| AObjGet -> -1
	| AObjSet -> -3
	| ALocalVar -> -1
	| ALocalAssign -> -2
	| AReturn -> -1
	| AGetURL2 _ -> -2
	| ADeleteObj | AInstanceOf | ACast -> -1
	| AExtends | AImplements -> -2
	| AEnum2 | ATrace | AThrow -> -1
	| AIncrement | ADecrement | AChr | AOrd | ARandom | ADelete | AGetTimer | ATypeOf | ATargetPath -> 0
	| AObjCall | ACall | ANewMethod -> assert false
	| AStringPool _ -> 0
	| op -> failwith ("Unknown stack delta for " ^ (ActionScript.action_string (fun _ -> "") 0 op))

let write ctx op =
	let write b op =
		DynArray.add ctx.opcodes op;
		ctx.code_pos <- ctx.code_pos + 1;
		ctx.stack_size <- ctx.stack_size + stack_delta op;
		ctx.opt_push <- b
	in
	match op with
	| APush l when ctx.opt_push ->
		(match DynArray.last ctx.opcodes with
		| (APush l2) as a ->
			ctx.code_pos <- ctx.code_pos - 1;
			ctx.stack_size <- ctx.stack_size - stack_delta a;
			DynArray.delete_last ctx.opcodes;
			write true (APush (l2 @ l))
		| _ ->
			assert false)
	| APush _ ->
		write true op
	| _ ->
		write false op

let call ctx kind n =
	let op , n = (match kind with
		| VarReg r ->
			write ctx (APush [PReg r;PUndefined]);
			AObjCall , n + 2
		| VarStr -> 
			ACall , n + 1
		| VarObj ->
			AObjCall , n + 2
	) in
	DynArray.add ctx.opcodes op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let new_call ctx kind n  =
	let op , n = (match kind with
		| VarReg r ->
			write ctx (APush [PReg r;PUndefined]);
			ANewMethod , n + 2
		| VarStr -> 
			ANew , n + 1
		| VarObj ->
			ANewMethod , n + 2
	) in
	DynArray.add ctx.opcodes op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let push ctx items =
	write ctx (APush (List.map (fun i ->
		match i with
		| VStr str ->
			let n = (try
				Hashtbl.find ctx.idents str
			with Not_found ->
				let n = ctx.ident_count in
				ctx.ident_count <- n + 1;
				Hashtbl.add ctx.idents str n;
				n
			) in
			if n <= 0xFF then 
				PStack n
			else
				PStack2 n
		| VInt n ->
			PInt (Int32.of_int n)
		| VInt32 n ->
			PInt n
		| VFloat f ->
			PDouble f
		| VThis ->
			PReg 1
		| VNull ->
			PNull
		| VSuper ->
			PReg 2
		| VReg n ->
			PReg n
	) items))

let rec pop ctx n =
	if n > 0 then begin
		write ctx APop;
		pop ctx (n-1);
	end

let cjmp ctx =
	write ctx (ACondJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		DynArray.set ctx.opcodes op_pos (ACondJump delta);
		ctx.opt_push <- false
	)

let jmp ctx =
	write ctx (AJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		DynArray.set ctx.opcodes op_pos (AJump delta);
		ctx.opt_push <- false
	)

let pos ctx =
	ctx.opt_push <- false;
	let start_pos = ctx.code_pos in
	(fun ~cond ->
		let delta = ctx.code_pos - start_pos in
		write ctx (if cond then ACondJump delta else AJump delta);
	)

let setvar ?(retval=false) ctx = function
	| VarReg (-1) -> assert false (** true, false, null **)
	| VarReg n -> write ctx (ASetReg n); if not retval then write ctx APop
	| VarStr
	| VarObj as s -> 
		if retval then write ctx (ASetReg 0);
		write ctx (if s = VarStr then ASet else AObjSet);
		if retval then push ctx [VReg 0]

let getvar ctx = function
	| VarReg (-1) -> () (** true, false, null **)
	| VarReg n -> push ctx [VReg n]
	| VarStr -> write ctx AEval
	| VarObj -> write ctx AObjGet

let func ctx constructor args =
	let default_flags = [ThisRegister; ArgumentsNoVar] in
	let f = {
		f2_name = "";
		f2_args = args;
		f2_codelen = 0;
		f2_nregs = 0;
		f2_flags = (if constructor then SuperRegister :: default_flags else SuperNoVar :: default_flags);
	} in
	write ctx (AFunction2 f);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun nregs ->
		let delta = ctx.code_pos - start_pos in
		f.f2_codelen <- delta;
		f.f2_nregs <- nregs
	)

(* -------------------------------------------------------------- *)
(* Generation Helpers *)

let idents_cache = Hashtbl.create 0

let gen_ident =
	let rand_char() =
		let n = Random.int 62 in
		if n < 26 then Char.chr (n + int_of_char 'a') else
		if n < 52 then Char.chr (n - 26 + int_of_char 'A') else
		Char.chr (n - 52 + int_of_char '0')
	in
	let rec loop() =
		let c = String.create 2 in
		c.[0] <- rand_char();
		c.[1] <- rand_char();
		if Hashtbl.mem idents_cache c then
			loop()
		else begin
			Hashtbl.add idents_cache c ();
			"@" ^ c
		end;
	in
	loop
		

let gen_type ctx t =
	try
		Hashtbl.find ctx.types t
	with
		Not_found ->
			let id = gen_ident() in
			Hashtbl.add ctx.types t id;
			id

(* -------------------------------------------------------------- *)
(* Generation *)

let gen_expr ctx e =
	assert false

let gen_class_static_field ctx f =
	match f.cf_expr with
	| None -> ()
	| Some e ->
		match e.eexpr with
		| TFunction _ ->
			push ctx [VReg 0; VStr f.cf_name];
			gen_expr ctx e;
			setvar ctx VarObj
		| _ ->
			assert false
	
let gen_class_field ctx f =
	match f.cf_expr with
	| None -> ()
	| Some e ->
		push ctx [VReg 1; VStr f.cf_name];
		gen_expr ctx e;
		setvar ctx VarObj

let gen_enum_field ctx f =
	let ename = mk (TConst (TString f.ef_name)) f.ef_type Ast.null_pos in
	push ctx [VReg 0; VStr f.ef_name];
	(match follow f.ef_type with
	| TFun (args,r) ->
		let n = ref (-1) in
		let args = List.map (fun t -> incr n; !n , "p" ^ string_of_int !n, t) args in
		let e = mk (TArrayDecl (ename :: 
			List.map (fun (_,n,t) -> mk (TLocal n) t Ast.null_pos) args
		)) r Ast.null_pos in
		let f = {
			tf_args = List.map (fun (_,n,t) -> n , t) args;
			tf_type = r;
			tf_expr = e;
		} in
		let f = func ctx false (List.map (fun (i,n,_) -> i ,n) args) in
		gen_expr ctx e;
		f (!n + 1)
	| t ->
		gen_expr ctx (mk (TArrayDecl [ename]) t Ast.null_pos));
	write ctx AObjSet

let gen_type_def ctx t tdef =
	match tdef with
	| TClassDecl c ->
		if c.cl_native then 
			()
		else
		let id = gen_type ctx t in
		push ctx [VStr id];
		(try 
			let constr = PMap.find "new" c.cl_statics in
			(match constr.cf_expr with
			| Some ({ eexpr = TFunction _ } as e) -> gen_expr ctx e
			| _ -> raise Not_found);
		with Not_found ->
			let f = func ctx true [] in
			f 0
		);
		write ctx (ASetReg 0);
		setvar ctx VarStr;
		push ctx [VReg 0; VStr "prototype"];
		getvar ctx VarObj;
		write ctx (ASetReg 1);
		write ctx APop;
		PMap.iter (fun _ f -> gen_class_static_field ctx f) c.cl_statics;
		PMap.iter (fun _ f -> gen_class_field ctx f) c.cl_fields;
	| TEnumDecl e ->
		let id = gen_type ctx t in
		push ctx [VStr id; VInt 0; VStr "Object"];
		write ctx ANew;
		write ctx (ASetReg 0);
		setvar ctx VarStr;
		PMap.iter (fun _ f -> gen_enum_field ctx f) e.e_constrs

let gen_type_map ctx =
	let packs = Hashtbl.create 0 in
	let rec loop acc cur = function
		| [] ->
			(if cur = "" then 
				VarStr
			else begin 
				push ctx [VStr cur];
				write ctx AEval;
				VarObj
			end)
		| p :: l ->
			let acc = p :: acc in
			try
				loop acc (Hashtbl.find packs acc) l
			with
				Not_found ->
					let id = (if cur = "" then
						p
					else begin
						let id = gen_ident() in
						push ctx [VStr id; VStr cur];
						write ctx AEval;
						push ctx [VStr p];
						write ctx AObjGet;
						write ctx ASet;
						id
					end) in
					Hashtbl.add packs acc id;
					push ctx [VStr id];
					write ctx AEval;
					let defined = cjmp ctx in
					push ctx [VStr id; VInt 0; VStr "Object"];
					write ctx ANew;
					write ctx ASet;
					if cur <> "" then begin
						push ctx [VStr cur];
						write ctx AEval;
						push ctx [VStr p; VStr id];
						write ctx AEval;
						write ctx AObjSet;
					end;
					defined();
					loop acc id l
	in
	Hashtbl.iter (fun (p,t) n ->
		let k = loop [] "" p in
		push ctx [VStr t;VStr n];
		write ctx AEval;
		setvar ctx k	
	) ctx.types

let to_utf8 str =
	try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code -> 
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UChar.of_char c)) str;
			UTF8.Buf.contents b

let generate file modules =
	let ctx = {
		opcodes = DynArray.create();
		code_pos = 0;
		stack_size = 0;
		ident_count = 0;
		opt_push = false;
		idents = Hashtbl.create 0;
		types = Hashtbl.create 0;
		locals = PMap.empty;
	} in
	write ctx (AStringPool []);
	List.iter (fun m ->
		List.iter (fun (p,t) -> gen_type_def ctx p t) m.mtypes
	) modules;
	gen_type_map ctx;
	let idents = ctx.idents in
	let idents = Hashtbl.fold (fun ident pos acc -> (ident,pos) :: acc) idents [] in
	let idents = List.sort (fun (_,p1) (_,p2) -> compare p1 p2) idents in
	DynArray.set ctx.opcodes 0 (AStringPool (List.map (fun (id,_) -> to_utf8 id) idents));
	let w = 400 in
	let h = 300 in
	let fps = 20. in
	let bg = 0xFFFFFF in
	let header = {
		h_version = 8;
		h_size = {
			rect_nbits = if (max w h) >= 820 then 16 else 15;
			left = 0;
			top = 0;
			right = w * 20;
			bottom = h * 20;
		};
		h_frame_count = 1;
		h_fps = to_float16 fps;
		h_compressed = true;
	} in
	let tag ?(ext=false) d = {
		tid = 0;
		textended = ext;
		tdata = d;
	} in
	let tagbg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
	let tagcode = tag (TDoAction ctx.opcodes) in
	let ch = IO.output_channel (open_out_bin file) in
	Swf.write ch (header,[tagbg;tagcode]);
	IO.close_out ch

;;
SwfParser.init SwfZip.inflate SwfZip.deflate;
Swf.warnings := false;
