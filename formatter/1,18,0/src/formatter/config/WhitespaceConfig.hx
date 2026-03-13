package formatter.config;

typedef WhitespaceConfig = {
	/**
		"(" + ")"
	**/
	@:default(auto) @:optional var parenConfig:ParenWhitespaceConfig;

	/**
		"{" + "}"
	**/
	@:default(auto) @:optional var bracesConfig:BracesWhitespaceConfig;

	/**
		"[" + "]"
	**/
	@:default(auto) @:optional var bracketConfig:BracketWhitespaceConfig;

	/**
		"<"
	**/
	@:default(None) @:optional var typeParamOpenPolicy:WhitespacePolicy;

	/**
		">"
	**/
	@:default(None) @:optional var typeParamClosePolicy:WhitespacePolicy;

	@:default(After) @:optional var typeExtensionPolicy:WhitespacePolicy;
	@:default(OnlyAfter) @:optional var commaPolicy:WhitespacePolicy;
	@:default(None) @:optional var dotPolicy:WhitespacePolicy;
	@:default(None) @:optional var colonPolicy:WhitespacePolicy;
	@:default(OnlyAfter) @:optional var caseColonPolicy:WhitespacePolicy;
	@:default(After) @:optional var objectFieldColonPolicy:WhitespacePolicy;
	@:default(None) @:optional var typeHintColonPolicy:WhitespacePolicy;
	@:default(Around) @:optional var typeCheckColonPolicy:WhitespacePolicy;
	@:default(Around) @:optional var ternaryPolicy:WhitespacePolicy;
	@:default(OnlyAfter) @:optional var semicolonPolicy:WhitespacePolicy;
	@:default(After) @:optional var ifPolicy:WhitespacePolicy;
	@:default(After) @:optional var doPolicy:WhitespacePolicy;
	@:default(After) @:optional var whilePolicy:WhitespacePolicy;
	@:default(After) @:optional var forPolicy:WhitespacePolicy;
	@:default(After) @:optional var switchPolicy:WhitespacePolicy;
	@:default(After) @:optional var tryPolicy:WhitespacePolicy;
	@:default(After) @:optional var catchPolicy:WhitespacePolicy;
	@:default(Around) @:optional var binopPolicy:WhitespacePolicy;
	@:default(None) @:optional var intervalPolicy:WhitespacePolicy;
	@:default(Around) @:optional var arrowFunctionsPolicy:WhitespacePolicy;
	@:default(None) @:optional var functionTypeHaxe3Policy:WhitespacePolicy;
	@:default(Around) @:optional var functionTypeHaxe4Policy:WhitespacePolicy;

	/**
		should formatter compress spaces for successive parenthesis `( [ {` vs. `([{`
	**/
	@:default(true) @:optional var compressSuccessiveParenthesis:Bool;

	/**
		should formatter try to format string interpolation expressions (e.g. '${x+3}' -> '${x + 3}')
		only applies spaces, no newlines or wrapping
	**/
	@:default(true) @:optional var formatStringInterpolation:Bool;

	/**
		ensure a space after '//'
	**/
	@:default(true) @:optional var addLineCommentSpace:Bool;
}

typedef ParenWhitespaceConfig = {
	/**
		parens used for metadata
	**/
	@:default(auto) @:optional var metadataParens:OpenClosePolicy;

	/**
		parens used for function parameters
	**/
	@:default(auto) @:optional var funcParamParens:OpenClosePolicy;

	/**
		parens used for anon function parameters
	**/
	@:default(auto) @:optional var anonFuncParamParens:OpenClosePolicy;

	/**
		parens used for calls
	**/
	@:default(auto) @:optional var callParens:OpenClosePolicy;

	/**
		parens used for conditions
	**/
	@:default({openingPolicy: NoneAfter, closingPolicy: OnlyAfter, removeInnerWhenEmpty: true}) @:optional var conditionParens:OpenClosePolicy;

	/**
		parens used for if conditions
	**/
	@:default(null) @:optional var ifConditionParens:OpenClosePolicy;

	/**
		parens used for switch conditions
	**/
	@:default(null) @:optional var switchConditionParens:OpenClosePolicy;

	/**
		parens used for while conditions
	**/
	@:default(null) @:optional var whileConditionParens:OpenClosePolicy;

	/**
		parens used for sharp conditions
	**/
	@:default(null) @:optional var sharpConditionParens:OpenClosePolicy;

	/**
		parens used for catch
	**/
	@:default(null) @:optional var catchParens:OpenClosePolicy;

	/**
		parens used for for loops
	**/
	@:default({openingPolicy: NoneAfter, closingPolicy: OnlyAfter, removeInnerWhenEmpty: true}) @:optional var forLoopParens:OpenClosePolicy;

	/**
		parens used for expressions
	**/
	@:default({openingPolicy: NoneAfter, closingPolicy: OnlyAfter, removeInnerWhenEmpty: true}) @:optional var expressionParens:OpenClosePolicy;
}

typedef BracesWhitespaceConfig = {
	/**
		braces for blocks
	**/
	@:default({openingPolicy: Before, closingPolicy: OnlyAfter, removeInnerWhenEmpty: true}) @:optional var blockBraces:OpenClosePolicy;

	/**
		braces for typdefs
	**/
	@:default({openingPolicy: Before, closingPolicy: OnlyAfter, removeInnerWhenEmpty: true}) @:optional var typedefBraces:OpenClosePolicy;

	/**
		braces for anon types
	**/
	@:default({openingPolicy: Before, closingPolicy: OnlyAfter, removeInnerWhenEmpty: true}) @:optional var anonTypeBraces:OpenClosePolicy;

	/**
		braces for object literals
	**/
	@:default({openingPolicy: Before, closingPolicy: OnlyAfter, removeInnerWhenEmpty: true}) @:optional var objectLiteralBraces:OpenClosePolicy;

	/**
		unknown braces
	**/
	@:default({openingPolicy: Before, closingPolicy: OnlyAfter, removeInnerWhenEmpty: true}) @:optional var unknownBraces:OpenClosePolicy;
}

typedef BracketWhitespaceConfig = {
	/**
		brackets for array access
	**/
	@:default({openingPolicy: None, closingPolicy: None, removeInnerWhenEmpty: true}) @:optional var accessBrackets:OpenClosePolicy;

	/**
		brackets for array and map comprehension
	**/
	@:default({openingPolicy: NoneAfter, closingPolicy: None, removeInnerWhenEmpty: true}) @:optional var comprehensionBrackets:OpenClosePolicy;

	/**
		brackets for array literals
	**/
	@:default({openingPolicy: NoneAfter, closingPolicy: None, removeInnerWhenEmpty: true}) @:optional var arrayLiteralBrackets:OpenClosePolicy;

	/**
		brackets for map literals
	**/
	@:default({openingPolicy: NoneAfter, closingPolicy: None, removeInnerWhenEmpty: true}) @:optional var mapLiteralBrackets:OpenClosePolicy;

	/**
		unknown brackets
	**/
	@:default({openingPolicy: NoneAfter, closingPolicy: None, removeInnerWhenEmpty: true}) @:optional var unknownBrackets:OpenClosePolicy;
}

typedef OpenClosePolicy = {
	/**
		"("
	**/
	@:default(None) @:optional var openingPolicy:WhitespacePolicy;

	/**
		")"
	**/
	@:default(OnlyAfter) @:optional var closingPolicy:WhitespacePolicy;

	/**
		"()" or "( )" - if `openingPolicy` contains `After` or `closingPolicy` contains `Before`
	**/
	@:default(true) @:optional var removeInnerWhenEmpty:Bool;
}
