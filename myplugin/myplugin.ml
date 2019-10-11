open EvalValue
open EvalEncode
open EvalContext
open MacroApi

let generate () =
	let com = (get_ctx()).curapi.get_com() in
	Genjs.generate com;
	vnull
;;
EvalStdLib.StdContext.register ["generate",vfun0 generate]