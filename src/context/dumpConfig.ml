type dump_mode =
	| NoDump
	| Ast
	| Pretty
	| Record
	| Position


type dump_stage =
	| AfterTyping
	| AfterCasting
	| AfterInlining
	| AfterAnalyzing
	| AfterSanitizing
	| AfterDce

type t = {
	mutable dump_mode : dump_mode;
	mutable dump_path : string;
	mutable dump_stage : dump_stage;
	mutable dump_print_ids : bool;
	mutable dump_dependencies : bool;
}

let create_default () = {
	dump_mode = NoDump;
	dump_path = "dump";
	dump_stage = AfterDce;
	dump_print_ids = false;
	dump_dependencies = false;
}

let update_from_defines conf def =
	conf.dump_mode <- begin match Define.defined_value_safe def Dump with
		| "1" -> Ast
		| "pretty" -> Pretty
		| "record" -> Record
		| "position" -> Position
		| _ -> NoDump
	end;
	conf.dump_path <- Define.defined_value_safe ~default:"dump" def DumpPath;
	conf.dump_stage <- begin match Define.defined_value_safe def DumpStage with
		| "typing" -> AfterTyping
		| "casting" -> AfterCasting
		| "inlining" -> AfterInlining
		| "analyzing" -> AfterAnalyzing
		| "sanitizing" -> AfterSanitizing
		| "dce" -> AfterDce
		| _ -> AfterDce
	end;
	conf.dump_print_ids <- not (Define.defined def Define.DumpIgnoreVarIds);
	conf.dump_dependencies <- Define.defined def Define.DumpDependencies

let string_of_dump_stage = function
	| AfterTyping -> "AfterTyping"
	| AfterCasting -> "AfterCasting"
	| AfterInlining -> "AfterInlining"
	| AfterAnalyzing -> "AfterAnalyzing"
	| AfterSanitizing -> "AfterSanitizing"
	| AfterDce -> "AfterDce"