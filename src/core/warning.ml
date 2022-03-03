type warning =
	(* general *)
	| WInternal
	| WInfo
	| WUser
	| WTemp
	(* subsystem *)
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

let warning_id = Obj.magic