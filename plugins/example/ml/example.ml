open EvalValue
open Type

class plugin =
	object (self)
		(**
			Prints greeting to stdout.
			Takes no arguments, returns Void.
		*)
		method hello () : value =
			print_endline "Hello from plugin";
			(*
				Plugin architecture requires to return something even for methods typed Void on Haxe side.
				Return `null`
			*)
			vnull
		(**
			Takes `haxe.macro.Position` and returns a string of that position in the same format used for
			compiler errors
		*)
		method stringify_position (pos:value) : value =
			let pos = EvalDecode.decode_pos pos in
			let str = Lexer.get_error_pos (Printf.sprintf "%s:%d:") pos in
			EvalEncode.encode_string str
		(**
			Change all static methods named "test" to throw "Hello from plugin".
			This is an example how to modify typed syntax tree.
		*)
		method hijack_static_test () : value =
			let compiler = (EvalContext.get_ctx()).curapi in
			(**
				Add a callback like `haxe.macro.Context.onAfterTyping`
			*)
			compiler.after_typing (fun haxe_types ->
				List.iter
					(fun hx_type ->
						match hx_type with
							| TClassDecl cls ->
								List.iter
									(fun field ->
										match field.cf_name, field.cf_expr with
											| "test", Some e ->
												let hello = {
													eexpr = TConst (TString "Hello from plugin");
													etype = (compiler.get_com()).basic.tstring;
													epos = Globals.null_pos;
												} in
												field.cf_expr <- Some { e with eexpr = TThrow hello }
											| _ -> ()
									)
									cls.cl_ordered_statics
							| _ -> ()
					)
					haxe_types
			);
			vnull
	end
;;

let api = new plugin in

(**
	Register our plugin API.
	This code is executed upon `eval.vm.Context.loadPlugin` call.
*)
EvalStdLib.StdContext.register [
	("hello", EvalEncode.vfun0 api#hello);
	("stringifyPosition", EvalEncode.vfun1 api#stringify_position);
	("hijackStaticTest", EvalEncode.vfun0 api#hijack_static_test);
]