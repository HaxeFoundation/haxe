open Globals
open Message
open Error
include WarningList

let parse_scoped_options s ps =
	let len = String.length s in
	let fail msg pmin pmax =
		raise_typing_error msg {ps with pmin = ps.pmin + pmin; pmax = ps.pmin + pmax}
	in
	let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
	let rec scan f i = if i < len && f s.[i] then scan f (i + 1) else i in
	let parse_value i =
		if i < len && s.[i] = '"' then begin match String.index_from_opt s (i + 1) '"' with
			| Some j -> String.sub s (i + 1) (j - i - 1),j + 1
			| None -> fail "Unterminated string" i len
		end else begin
			let j = scan (fun c -> not (is_space c)) i in
			String.sub s i (j - i),j
		end
	in
	let parse_scope i =
		let j = scan SourceScope.is_ident i in
		let value,k = if j < len && s.[j] = ':' then (let v,k = parse_value (j + 1) in Some v,k) else None,j in
		try
			SourceScope.make (String.sub s i (j - i)) value,k
		with
		| SourceScope.Invalid msg -> fail msg i k
		| SourceScope.Unknown_kind -> fail SourceScope.expected i (max j (i + 1))
	in
	let rec next acc i =
		let i = scan is_space i in
		if i >= len then
			List.rev acc
		else match s.[i] with
			| '+' | '-' as c ->
				let j = scan SourceScope.is_ident (i + 1) in
				let name = String.sub s (i + 1) (j - i - 1) in
				if name = "" then fail "Expected identifier" (i + 1) (i + 2);
				let warning = try from_string name with Exit -> fail (Printf.sprintf "Unknown warning: %s" name) (i + 1) j in
				let scope,k = if j < len && s.[j] = '@' then (let scope,k = parse_scope (j + 1) in Some scope,k) else None,j in
				let mode = if c = '+' then WMEnable else WMDisable in
				next (({ wo_warning = warning; wo_mode = mode },scope) :: acc) k
			| _ ->
				fail "Expected + or -" i (i + 1)
	in
	next [] 0

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

let rec matches w id =
	id = w || match (warning_obj w).w_parent with
		| None -> false
		| Some w' -> matches w' id

let get_mode w (l : warning_option list list) =
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
		if matches w opt.wo_warning && in_scope scope then Some opt.wo_mode else matches_scoped w scoped in_scope
