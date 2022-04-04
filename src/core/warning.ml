open Globals
open Error
include WarningList

type warning_range =
	| WRExact of int
	| WRRange of int * int

type warning_mode =
	| WMEnable
	| WMDisable

type warning_option = {
	wo_range : warning_range;
	wo_mode  : warning_mode;
}

let parse_options s ps lexbuf =
	let fail msg p =
		Error.typing_error msg {p with pmin = ps.pmin + p.pmin; pmax = ps.pmin + p.pmax}
	in
	let parse_string s p =
		begin try
			warning_id (from_string s)
		with Exit ->
			fail (Printf.sprintf "Unknown warning: %s" s) p
		end
	in
	let parse_range () = match Lexer.token lexbuf with
		| Const (Int(i,_)),_ ->
			WRExact (int_of_string i)
		| Const (Ident s),p ->
			WRExact (parse_string s p)
		| IntInterval i1,_ ->
			begin match Lexer.token lexbuf with
			| Const (Int(i2,_)),_ ->
				WRRange(int_of_string i1,int_of_string i2)
			| (_,p) ->
				fail "Expected number" p
			end
		| (_,p) ->
			fail "Expected number or identifier" p
	in
	let add acc mode range =
		{ wo_range = range; wo_mode = mode } :: acc
	in
	let rec next acc = match Lexer.token lexbuf with
		| Binop OpAdd,_ ->
			next (add acc WMEnable (parse_range()))
		| Binop OpSub,_ ->
			next (add acc WMDisable (parse_range()))
		| Eof,_ ->
			List.rev acc
		| (_,p) ->
			fail "Expected + or -" p
	in
	next []

let parse_options s ps =
	let restore = Lexer.reinit ps.pfile in
	Std.finally (fun () ->
		restore()
	) (fun () ->
		let lexbuf = Sedlexing.Utf8.from_string s in
		parse_options s ps lexbuf
	) ()

let from_meta ml =
	let parse_arg e = match fst e with
		| Ast.EConst (String(s,_)) ->
			let p = snd e in
			parse_options s {p with pmin = p.pmin + 1; pmax = p.pmax - 1} (* pmin is on the quote *)
		| _ ->
			Error.typing_error "String expected" (snd e)
	in
	let rec loop acc ml = match ml with
		| (Meta.HaxeWarning,args,_) :: ml ->
			let acc = List.fold_left (fun acc arg ->
				(parse_arg arg) :: acc
			) acc args in
			loop acc ml
		| _ :: ml ->
			loop acc ml
		| [] ->
			List.rev acc
	in
	loop [] ml

let get_mode w (l : warning_option list list) =
	let code = warning_id w in
	let in_range range = match range with
		| WRExact i -> i = code
		| WRRange(i1,i2) -> code >= i1 && code <= i2
	in
	let rec loop mode l = match l with
		| [] ->
			mode
		| l2 :: l ->
			let rec loop2 mode l = match l with
				| [] ->
					mode
				| opt :: l ->
					let mode = if in_range opt.wo_range then opt.wo_mode else mode in
					loop2 mode l
			in
			loop (loop2 mode l2) l
	in
	loop WMEnable (* ? *) l