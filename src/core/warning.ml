type warning =
	(* general *)
	| WInternal
	| WInfo
	| WUser
	| WTemp
	(* subsystem *)
	| WTyper
	| WMatcher
	| WMacro
	| WAnalyzer
	| WInliner
	| WGencommon
	| WGenerator
	(* specific *)
	| WDeprecated
	| WVarShadow
	| WExternInit
	| WStaticInitOrder
	| WClosureCompare
	| WVarInit
	| WReservedTypePath

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

let warning_id = function
	| WInternal -> 0
	| WInfo -> 1
	| WUser -> 2
	| WTemp -> 3
	(* subsystem *)
	| WTyper -> 100
	| WMacro -> 200
	| WMatcher -> 300
	| WInliner -> 400
	| WAnalyzer -> 500
	| WGencommon -> 600
	| WGenerator -> 700
	(* specific *)
	| WDeprecated -> 101
	| WVarInit -> 102
	| WVarShadow -> 103
	| WExternInit -> 104
	| WStaticInitOrder -> 105
	| WClosureCompare -> 106
	| WReservedTypePath -> 107

let parse_options s =
	let lexbuf = Sedlexing.Utf8.from_string s in
	let fail msg =
		raise (Failure msg)
	in
	let id () = match Lexer.token lexbuf with
		| Const (Int(i,_)),_ ->
			WRExact (int_of_string i)
		| IntInterval i1,_ ->
			begin match Lexer.token lexbuf with
			| Const (Int(i2,_)),_ ->
				WRRange(int_of_string i1,int_of_string i2)
			| _ ->
				fail "Expected number"
			end
		| _ ->
			fail "Expected number"
	in
	let parse_range () =
		try
			id()
		with Failure msg ->
			fail msg
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
		| _ ->
			fail "Expected + or -"
	in
	next []

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