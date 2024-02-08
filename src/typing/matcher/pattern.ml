open Globals
open Type
open MatcherGlobals

type t =
	| PatConstructor of Constructor.t * pattern list
	| PatAny
	| PatBind of tvar * pattern
	| PatOr of pattern * pattern
	| PatTuple of pattern list
	| PatExtractor of extractor

and pattern = t * pos

and extractor = {
	ex_var     : tvar; (* The _ local used for typing the expression *)
	ex_expr    : texpr; (* The left side of the => *)
	ex_pattern : pattern; (* The right side of the => *)
}

let rec to_string pat = match fst pat with
	| PatConstructor(con,patterns) -> Printf.sprintf "%s(%s)" (Constructor.to_string con) (String.concat ", " (List.map to_string patterns))
	| PatAny -> "_"
	| PatBind(v,(PatAny,_)) -> Printf.sprintf "%s<%i>" v.v_name v.v_id
	| PatBind(v,pat1) -> Printf.sprintf "%s = %s" v.v_name (to_string pat1)
	| PatOr(pat1,pat2) -> Printf.sprintf "(%s) | (%s)" (to_string pat1) (to_string pat2)
	| PatTuple pl -> Printf.sprintf "[%s]" (String.concat ", " (List.map to_string pl))
	| PatExtractor ex -> Printf.sprintf "%s => %s" (s_expr_pretty ex.ex_expr) (to_string ex.ex_pattern)
