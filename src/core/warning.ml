open Globals
open Message
open Error
include WarningList

let parse_scoped_options s ps =
	let lctx = Lexer.create_temp_ctx ps.pfile in
	let lexbuf = Sedlexing.Utf8.from_string s in
	let fail msg p =
		raise_typing_error msg {p with pmin = ps.pmin + p.pmin; pmax = ps.pmin + p.pmax}
	in
	let rec tokens acc = match Lexer.token lctx lexbuf with
		| (Eof,_) as tk -> List.rev (tk :: acc)
		| tk -> tokens (tk :: acc)
	in
	let parse_warning (tl : (Ast.token * pos) list) = match tl with
		| (Const (Ident s),p) :: tl ->
			begin try
				from_string s,tl
			with Exit ->
				fail (Printf.sprintf "Unknown warning: %s" s) p
			end
		| (_,p) :: _ ->
			fail "Expected identifier" p
		| [] ->
			die "" __LOC__
	in
	let parse_scope (tl : (Ast.token * pos) list) = match tl with
		| (At,_) :: (Const (Ident "user"),_) :: tl -> Some WSUser,tl
		| (At,_) :: (Const (Ident "std"),_) :: tl -> Some WSStd,tl
		| (At,_) :: (Const (Ident "lib"),_) :: (DblDot,_) :: (Const (Ident s | String(s,_)),_) :: tl -> Some (WSLib (Some s)),tl
		| (At,_) :: (Const (Ident "lib"),_) :: tl -> Some (WSLib None),tl
		| (At,_) :: (Const (String(s,_)),_) :: tl -> Some (WSPath s),tl
		| (At,_) :: (_,p) :: _ -> fail "Expected user, lib, lib:name, std or a path string" p
		| tl -> None,tl
	in
	let rec next acc (tl : (Ast.token * pos) list) = match tl with
		| (Binop (OpAdd | OpSub as op),_) :: tl ->
			let warning,tl = parse_warning tl in
			let scope,tl = parse_scope tl in
			let mode = if op = OpAdd then WMEnable else WMDisable in
			next (({ wo_warning = warning; wo_mode = mode },scope) :: acc) tl
		| (Eof,_) :: _ | [] ->
			List.rev acc
		| (_,p) :: _ ->
			fail "Expected + or -" p
	in
	next [] (tokens [])

let parse_options s ps =
	List.map (fun (opt,scope) -> match scope with
		| None -> opt
		| Some _ -> raise_typing_error "Warning scopes (@) are only supported by -w" ps
	) (parse_scoped_options s ps)

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

let rec matches_scoped w scoped in_scope = match scoped with
	| [] ->
		None
	| (opt,scope) :: scoped ->
		let rec matches w id =
			id = w || match (warning_obj w).w_parent with
				| None -> false
				| Some w' -> matches w' id
		in
		if matches w opt.wo_warning && in_scope scope then Some opt.wo_mode else matches_scoped w scoped in_scope
