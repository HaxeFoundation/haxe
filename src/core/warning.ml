open Globals
open Error
include WarningList

type warning_mode =
	| WMEnable
	| WMDisable

type warning_option = {
	wo_warning : warning;
	wo_mode : warning_mode;
}

let parse_options s ps lexbuf =
	let fail msg p =
		raise_typing_error msg {p with pmin = ps.pmin + p.pmin; pmax = ps.pmin + p.pmax}
	in
	let parse_string s p =
		begin try
			from_string s
		with Exit ->
			fail (Printf.sprintf "Unknown warning: %s" s) p
		end
	in
	let parse_warning () = match Lexer.token lexbuf with
		| Const (Ident s),p ->
			parse_string s p
		| (_,p) ->
			fail "Expected identifier" p
	in
	let add acc mode warning =
		{ wo_warning = warning; wo_mode = mode } :: acc
	in
	let rec next acc = match Lexer.token lexbuf with
		| Binop OpAdd,_ ->
			next (add acc WMEnable (parse_warning()))
		| Binop OpSub,_ ->
			next (add acc WMDisable (parse_warning()))
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
			raise_typing_error "String expected" (snd e)
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
	let rec matches w id =
		id = w || match (warning_obj w).w_parent with
			| None -> false
			| Some w' -> matches w' id
	in
	let rec loop l = match l with
		| [] ->
			WMEnable
		| l2 :: l ->
			let rec loop2 l = match l with
				| [] ->
					None
				| opt :: l ->
					if matches w opt.wo_warning then Some opt.wo_mode else loop2 l
			in
			match loop2 l2 with
			| None ->
				loop l
			| Some mode ->
				mode
	in
	loop l
