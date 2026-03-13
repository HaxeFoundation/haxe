package formatter.config;

typedef EmptyLinesConfig = {
	/**
		adds a final newline
	**/
	@:default(true) @:optional var finalNewline:Bool;

	/**
		maximum consecutive empty lines anywhere in file - runs last, all empty lines policies are limited to "maxAnywhereInFile"
	**/
	@:default(1) @:optional var maxAnywhereInFile:Int;

	/**
		empty lines before package declaration
	**/
	@:default(0) @:optional var beforePackage:Int;

	/**
		empty lines after package declaration
	**/
	@:default(1) @:optional var afterPackage:Int;

	/**
		empty lines for package, import and using section
	**/
	@:default(auto) @:optional var importAndUsing:ImportsEmptyLinesConfig;

	/**
		empty lines between types
	**/
	@:default(1) @:optional var betweenTypes:Int;

	/**
		empty lines for line comments between types
	**/
	@:default(Keep) @:optional var lineCommentsBetweenTypes:LineCommentEmptyLinePolicy;

	/**
		empty lines for line comments between functions
	**/
	@:default(Keep) @:optional var lineCommentsBetweenFunctions:LineCommentEmptyLinePolicy;

	/**
		empty lines between two single line types
	**/
	@:default(0) @:optional var betweenSingleLineTypes:Int;

	/**
		Remove or keep empty lines above "}"
	**/
	@:default(Remove) @:optional var beforeRightCurly:KeepEmptyLinesPolicy;

	/**
		Remove or keep empty lines below "{"
	**/
	@:default(Remove) @:optional var afterLeftCurly:KeepEmptyLinesPolicy;

	/**
		Remove or keep empty lines below "return"
	**/
	@:default(Remove) @:optional var afterReturn:KeepEmptyLinesPolicy;

	@:default(Remove) @:optional var beforeBlocks:KeepEmptyLinesPolicy;
	@:default(Remove) @:optional var afterBlocks:KeepEmptyLinesPolicy;
	@:default(auto) @:optional var enumAbstractEmptyLines:EnumAbstractFieldsEmptyLinesConfig;
	@:default(auto) @:optional var classEmptyLines:ClassFieldsEmptyLinesConfig;
	@:default(auto) @:optional var macroClassEmptyLines:ClassFieldsEmptyLinesConfig;
	@:default(auto) @:optional var externClassEmptyLines:InterfaceFieldsEmptyLinesConfig;
	@:default(auto) @:optional var abstractEmptyLines:ClassFieldsEmptyLinesConfig;
	@:default(auto) @:optional var interfaceEmptyLines:InterfaceFieldsEmptyLinesConfig;
	@:default(auto) @:optional var enumEmptyLines:TypedefFieldsEmptyLinesConfig;
	@:default(auto) @:optional var typedefEmptyLines:TypedefFieldsEmptyLinesConfig;
	@:default(auto) @:optional var conditionalsEmptyLines:ConditionalEmptyLinesConfig;

	/**
		"one" adds one empty line above doc comments
		"none" removes all empty lines above doc comments
		"ignore" respects empty lines set via "betweenVars", "betweenFunctions", etc.
	**/
	@:default(One) @:optional var beforeDocCommentEmptyLines:CommentEmptyLinesPolicy;

	/**
		"one" adds one empty line after fields with doc comments
		"none" removes all empty lines line fields with doc comments
		"ignore" respects empty lines set via "betweenVars", "betweenFunctions", etc.
	**/
	@:default(One) @:optional var afterFieldsWithDocComments:CommentEmptyLinesPolicy;

	/**
		Adds empty lines when file starts with a multiline comment
	**/
	@:default(1) @:optional var afterFileHeaderComment:Int;

	/**
		Adds empty lines between two consecutive multiline comments
	**/
	@:default(0) @:optional var betweenMultilineComments:Int;
}

typedef ConditionalEmptyLinesConfig = {
	@:default(0) @:optional var afterIf:Int;
	@:default(0) @:optional var beforeElse:Int;
	@:default(0) @:optional var afterElse:Int;
	@:default(0) @:optional var beforeEnd:Int;
	@:default(0) @:optional var beforeError:Int;
	@:default(0) @:optional var afterError:Int;
}

typedef ClassFieldsEmptyLinesConfig = {
	/**
		add empty lines after opening "{" of type
	**/
	@:default(0) @:optional var beginType:Int;

	/**
		add empty lines before closing "}" of type
	**/
	@:default(0) @:optional var endType:Int;

	@:default(0) @:optional var betweenStaticVars:Int;
	@:default(1) @:optional var afterStaticVars:Int;
	@:default(1) @:optional var afterPrivateVars:Int;
	@:default(0) @:optional var betweenVars:Int;
	@:default(1) @:optional var afterVars:Int;
	@:default(1) @:optional var afterStaticFunctions:Int;
	@:default(1) @:optional var betweenStaticFunctions:Int;
	@:default(1) @:optional var afterPrivateFunctions:Int;
	@:default(1) @:optional var betweenFunctions:Int;
	@:default(Keep) @:optional var existingBetweenFields:KeepEmptyLinesPolicy;
}

typedef EnumAbstractFieldsEmptyLinesConfig = {
	/**
		add empty lines after opening "{" of type
	**/
	@:default(0) @:optional var beginType:Int;

	/**
		add empty lines before closing "}" of type
	**/
	@:default(0) @:optional var endType:Int;

	@:default(0) @:optional var betweenVars:Int;
	@:default(1) @:optional var afterVars:Int;
	@:default(1) @:optional var betweenFunctions:Int;
	@:default(Keep) @:optional var existingBetweenFields:KeepEmptyLinesPolicy;
}

typedef InterfaceFieldsEmptyLinesConfig = {
	/**
		add empty lines after opening "{" of type
	**/
	@:default(0) @:optional var beginType:Int;

	/**
		add empty lines before closing "}" of type
	**/
	@:default(0) @:optional var endType:Int;

	@:default(0) @:optional var betweenVars:Int;
	@:default(0) @:optional var afterVars:Int;
	@:default(0) @:optional var betweenFunctions:Int;
	@:default(Keep) @:optional var existingBetweenFields:KeepEmptyLinesPolicy;
}

typedef TypedefFieldsEmptyLinesConfig = {
	/**
		add empty lines after opening "{" of type
	**/
	@:default(0) @:optional var beginType:Int;

	/**
		add empty lines before closing "}" of type
	**/
	@:default(0) @:optional var endType:Int;

	@:default(0) @:optional var betweenFields:Int;
	@:default(Keep) @:optional var existingBetweenFields:KeepEmptyLinesPolicy;
}

enum abstract KeepEmptyLinesPolicy(String) {
	var Keep = "keep";
	var Remove = "remove";
}

enum abstract CommentEmptyLinesPolicy(String) {
	var Ignore = "ignore";
	var None = "none";
	var One = "one";
}

typedef ImportsEmptyLinesConfig = {
	/**
		empty lines between two consecutive import/using lines using betweenImportsLevel
	**/
	@:default(0) @:optional var betweenImports:Int;

	/**
		restrict betweenImports setting to a specific level
		"all" - apply betweenImports to all imports/using entries
		"firstLevelPackage" - group imports/using entries using toplevel package names; no empty lines for identical toplevel names
		"secondLevelPackage" - group imports/using entries using top- and secondlevel package names; no empty lines for identical toplevel and secondlevel names
		"thirdLevelPackage" - group imports/using entries using upto thirdlevel package names
		"fourthLevelPackage" - group imports/using entries using upto foruthlevel package names
		"fifthLevelPackage" - group imports/using entries using upto fifthlevel package names
		"fullPackage" - group imports/using entries using full packages; no empty lines for identical package names
	**/
	@:default(All) @:optional var betweenImportsLevel:BetweenImportsEmptyLinesLevel;

	/**
		empty lines between import and using lines
	**/
	@:default(1) @:optional var beforeUsing:Int;

	/**
		empty lines after import and using section
	**/
	@:default(1) @:optional var beforeType:Int;
}

enum abstract BetweenImportsEmptyLinesLevel(String) {
	var All = "all";
	var FirstLevelPackage = "firstLevelPackage";
	var SecondLevelPackage = "secondLevelPackage";
	var ThirdLevelPackage = "thirdLevelPackage";
	var FourthLevelPackage = "fourthLevelPackage";
	var FifthLevelPackage = "fifthLevelPackage";
	var FullPackage = "fullPackage";
}

enum abstract LineCommentEmptyLinePolicy(String) {
	var Keep = "keep";
	var One = "one";
	var None = "none";
}
