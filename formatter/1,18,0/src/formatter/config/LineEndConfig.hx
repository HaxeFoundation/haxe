package formatter.config;

typedef LineEndConfig = {
	/**
		line end character to use when outputting formatted code
	**/
	@:default(Auto) @:optional var lineEndCharacter:LineEndCharacter;

	@:default(None) @:optional var metadataType:AtLineEndPolicy;
	@:default(None) @:optional var metadataVar:AtLineEndPolicy;
	@:default(None) @:optional var metadataFunction:AtLineEndPolicy;
	@:default(None) @:optional var metadataOther:AtLineEndPolicy;
	@:default(After) @:optional var caseColon:CaseColonLineEndPolicy;
	@:default(After) @:optional var sharp:SharpLineEndPolicy;

	/**
		global left curly line end setting
	**/
	@:default(After) @:optional var leftCurly:LeftCurlyLineEndPolicy;

	/**
		global right curly line end setting
	**/
	@:default(Both) @:optional var rightCurly:RightCurlyLineEndPolicy;

	/**
		global empty curlies line end setting
	**/
	@:default(NoBreak) @:optional var emptyCurly:EmptyCurlyPolicy;

	/**
		line end settings for block curlies
	**/
	@:default(null) @:optional var blockCurly:Null<CurlyLineEndPolicy>;

	/**
		line end settings for anon function body curlies
	**/
	@:default(null) @:optional var anonFunctionCurly:Null<CurlyLineEndPolicy>;

	/**
		line end settings for anon type curlies
	**/
	@:default(null) @:optional var anonTypeCurly:Null<CurlyLineEndPolicy>;

	/**
		line end settings for object literal curlies
	**/
	@:default(null) @:optional var objectLiteralCurly:Null<CurlyLineEndPolicy>;

	/**
		line end settings for typedef curlies
	**/
	@:default(null) @:optional var typedefCurly:Null<CurlyLineEndPolicy>;
}

enum abstract AtLineEndPolicy(String) {
	var None = "none";
	var After = "after";
	var AfterLast = "afterLast";
	var ForceAfterLast = "forceAfterLast";
}

enum abstract CaseColonLineEndPolicy(String) {
	var None = "none";
	var After = "after";
}

enum abstract SharpLineEndPolicy(String) {
	var None = "none";
	var After = "after";
}

typedef CurlyLineEndPolicy = {
	/**
		line end options for left curly
	**/
	@:default(After) @:optional var leftCurly:LeftCurlyLineEndPolicy;

	/**
		line end options for right curly
	**/
	@:default(Both) @:optional var rightCurly:RightCurlyLineEndPolicy;

	/**
		line end options for empty curlies
	**/
	@:default(NoBreak) @:optional var emptyCurly:EmptyCurlyPolicy;
}

enum abstract LeftCurlyLineEndPolicy(String) {
	var None = "none";
	var After = "after";
	var Before = "before";
	var Both = "both";
}

enum abstract RightCurlyLineEndPolicy(String) {
	var None = "none";
	var Before = "before";
	var After = "after";
	var Both = "both";
}

enum abstract EmptyCurlyPolicy(String) {
	var NoBreak = "noBreak";
	var Break = "break";
}

enum abstract LineEndCharacter(String) {
	/**
		detect line end character from input (repeated for each input file)
	**/
	var Auto = "auto";

	/**
		output files with \r line endings
	**/
	var CR = "CR";

	/**
		output files with \n line endings
	**/
	var LF = "LF";

	/**
		output files with \r\n line endings
	**/
	var CRLF = "CRLF";
}
