package formatter.config;

typedef SameLineConfig = {
	/**
		same line policy for non block body of "if"
		* same = place if and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Next) @:optional var ifBody:SameLinePolicy;

	/**
		same line policy for non block body of "else"
		* same = place else and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Next) @:optional var elseBody:SameLinePolicy;

	/**
		same line policy for "else" part of "if…else"
		* same = place else and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var ifElse:SameLinePolicy;

	/**
		Add new line after `if body` to `if (...) body; else ...`
		Don't touch `if (...) body else ...` or `if (...) {body} else ...`
	**/
	@:default(true) @:optional var ifElseSemicolonNextLine:Bool;

	/**
		same line policy for "if" part of "else if"
		* same = place if and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var elseIf:SameLinePolicy;

	/**
		same line policy for non block body of "if" in a value place / as expression
		* same = place if and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var expressionIf:SameLinePolicy;

	/**
		will place if with one expression in a block in one line (requires "expressionIf" = "same")
		var foo = if (bar) { ""; } else { ""; };
	**/
	@:default(false) @:optional var expressionIfWithBlocks:Bool;

	/**
		same line policy for non block body of "for"
		* same = place function and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Next) @:optional var forBody:SameLinePolicy;

	@:default(Same) @:optional var comprehensionFor:SameLinePolicy;

	/**
		same line policy for non block body of "while" (not "do…while")
		* same = place while and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Next) @:optional var whileBody:SameLinePolicy;

	/**
		same line policy for non block body of "do…while"
		* same = place function and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Next) @:optional var doWhileBody:SameLinePolicy;

	/**
		same line policy for "while" part in "do…while"
		* same = place while and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var doWhile:SameLinePolicy;

	/**
		same line policy for non block body of "try"
		* same = place try and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Next) @:optional var tryBody:SameLinePolicy;

	/**
		same line policy for non block body of "catch"
		* same = place catch and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Next) @:optional var catchBody:SameLinePolicy;

	/**
		same line policy for "catch" part of "try…catch"
		* same = place function and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var tryCatch:SameLinePolicy;

	@:default(Next) @:optional var caseBody:SameLinePolicy;
	@:default(Keep) @:optional var expressionCase:SameLinePolicy;
	@:default(Same) @:optional var expressionTry:SameLinePolicy;

	/**
		same line policy for non block body of "function"
		* same = place function and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Next) @:optional var functionBody:SameLinePolicy;

	/**
		same line policy for non block body of anon "function"
		* same = place function and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var anonFunctionBody:SameLinePolicy;

	/**
		same line policy for multiline expression return values
		* same = place return and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var returnBody:SameLinePolicy;

	/**
		same line policy for single line expression return values
		* same = place return and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var returnBodySingleLine:SameLinePolicy;

	/**
		same line policy for untyped {…} as a body
		* same = place return and body on same line
		* next = place body on next line
		* keep = keep same / next line from source
	**/
	@:default(Same) @:optional var untypedBody:SameLinePolicy;
}

enum abstract SameLinePolicy(String) {
	var Same = "same";
	var Next = "next";
	var Keep = "keep";
}
