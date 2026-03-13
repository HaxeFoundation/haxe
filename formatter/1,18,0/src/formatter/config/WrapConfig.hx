package formatter.config;

typedef WrapConfig = {
	/**
		maximum characters per line, formatter will try to wrap code longer than `maxLineLength`
	**/
	@:default(160) @:optional var maxLineLength:Int;

	/**
		array wrapping rules
		does not affect array comprehension, use "sameLine.comprehensionFor"
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: HasMultiLineItems, value: 1}],
				type: OnePerLine
			},
			{
				conditions: [{cond: TotalItemLengthLessThan, value: 80}],
				type: NoWrap
			},
			{
				conditions: [
					{cond: EqualItemLengths, value: 1},
					{cond: AllItemLengthsLessThan, value: 30},
					{cond: ItemCountLargerThan, value: 10}
				],
				type: FillLineWithLeadingBreak
			},
			{
				conditions: [
					{cond: AllItemLengthsLessThan, value: 10},
					{cond: ItemCountLargerThan, value: 10}
				],
				type: FillLineWithLeadingBreak
			},
			{
				conditions: [{cond: AnyItemLengthLargerThan, value: 30}],
				type: OnePerLine
			},
			{
				conditions: [{cond: ItemCountLargerThan, value: 4}],
				type: OnePerLine
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: OnePerLine
			}
		]
	})
	@:optional
	var arrayWrap:WrapRules;

	/**
		map literal wrapping rules
		does not affect map comprehension, use "sameLine.comprehensionFor"
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: HasMultiLineItems, value: 1}],
				type: OnePerLine
			},
			{
				conditions: [{cond: TotalItemLengthLessThan, value: 80}],
				type: NoWrap
			},
			{
				conditions: [
					{cond: EqualItemLengths, value: 1},
					{cond: AllItemLengthsLessThan, value: 30},
					{cond: ItemCountLargerThan, value: 10}
				],
				type: FillLineWithLeadingBreak
			},
			{
				conditions: [
					{cond: AllItemLengthsLessThan, value: 10},
					{cond: ItemCountLargerThan, value: 10}
				],
				type: FillLineWithLeadingBreak
			},
			{
				conditions: [{cond: AnyItemLengthLargerThan, value: 30}],
				type: OnePerLine
			},
			{
				conditions: [{cond: ItemCountLargerThan, value: 4}],
				type: OnePerLine
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: OnePerLine
			}
		]
	})
	@:optional
	var mapWrap:WrapRules;

	/**
		detect arrays in matrix configuration from source
		noMatrixWrap = no detection
		matrixWrapNoAlign = detect and format as matrix without alignment
		matrixWrapWithAlign = detect and format as matrix and align columns
	**/
	@:default(MatrixWrapWithAlign) @:optional var arrayMatrixWrap:ArrayMatrixWrap;

	/**
		type parameter wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: AnyItemLengthLargerThan, value: 50}],
				type: FillLine
			},
			{
				conditions: [{cond: TotalItemLengthLargerThan, value: 70}],
				type: FillLine
			}
		]
	})
	@:optional
	var typeParameter:WrapRules;

	/**
		named function signature wrapping rules
	**/
	@:default({
		defaultWrap: FillLine,
		defaultAdditionalIndent: 1,
		rules: []
	})
	@:optional
	var functionSignature:WrapRules;

	/**
		anon function signature wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: ItemCountLargerThan, value: 7}],
				type: FillLine,
				additionalIndent: 1
			},
			{
				conditions: [{cond: TotalItemLengthLargerThan, value: 80}],
				type: FillLine,
				additionalIndent: 1
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: FillLine,
				additionalIndent: 1
			}
		]
	})
	@:optional
	var anonFunctionSignature:WrapRules;

	/**
		call parameter wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: ItemCountLargerThan, value: 7}],
				type: FillLine
			},
			{
				conditions: [{cond: TotalItemLengthLargerThan, value: 140}],
				type: FillLine
			},
			{
				conditions: [{cond: AnyItemLengthLargerThan, value: 80}],
				type: FillLine
			},
			{
				conditions: [{cond: LineLengthLargerThan, value: 160}],
				type: FillLine
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: FillLine
			}
		]
	})
	@:optional
	var callParameter:WrapRules;

	/**
		metadata call parameter wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: TotalItemLengthLargerThan, value: 140}],
				type: FillLine
			},
			{
				conditions: [{cond: LineLengthLargerThan, value: 160}],
				type: FillLine
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: FillLine
			}
		]
	})
	@:optional
	var metadataCallParameter:WrapRules;

	/**
		object literal wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: ItemCountLessThan, value: 3}, {cond: ExceedsMaxLineLength, value: 0}],
				type: NoWrap
			},
			{
				conditions: [{cond: AnyItemLengthLargerThan, value: 30}],
				type: OnePerLine
			},
			{
				conditions: [{cond: TotalItemLengthLargerThan, value: 60}],
				type: OnePerLine
			},
			{
				conditions: [{cond: ItemCountLargerThan, value: 4}],
				type: OnePerLine
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: OnePerLine
			}
		]
	})
	@:optional
	var objectLiteral:WrapRules;

	/**
		anon types wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: ItemCountLessThan, value: 3}, {cond: ExceedsMaxLineLength, value: 0}],
				type: NoWrap
			},
			{
				conditions: [{cond: AnyItemLengthLargerThan, value: 30}],
				type: OnePerLine
			},
			{
				conditions: [{cond: TotalItemLengthLargerThan, value: 60}],
				type: OnePerLine
			},
			{
				conditions: [{cond: ItemCountLargerThan, value: 4}],
				type: OnePerLine
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: FillLine
			}
		]
	})
	@:optional
	var anonType:WrapRules;

	/**
		method chaining wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: LineLengthLargerThan, value: 160}],
				type: OnePerLineAfterFirst
			},
			{
				conditions: [{cond: ItemCountLessThan, value: 3}, {cond: ExceedsMaxLineLength, value: 0}],
				type: NoWrap
			},
			{
				conditions: [
					{cond: TotalItemLengthLessThan, value: 80},
					{cond: ExceedsMaxLineLength, value: 0}
				],
				type: NoWrap
			},
			{
				conditions: [
					{cond: AnyItemLengthLargerThan, value: 30},
					{cond: ItemCountLargerThan, value: 4}
				],
				type: OnePerLineAfterFirst
			},
			{
				conditions: [{cond: ItemCountLargerThan, value: 7}],
				type: OnePerLineAfterFirst
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: OnePerLineAfterFirst
			}
		]
	})
	@:optional
	var methodChain:WrapRules;

	/**
		OpBool chain wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [
					{cond: LineLengthLargerThan, value: 140},
					{cond: AnyItemLengthLargerThan, value: 40}
				],
				location: BeforeLast,
				type: OnePerLineAfterFirst
			},
			{
				conditions: [{cond: LineLengthLargerThan, value: 140}],
				location: BeforeLast,
				type: FillLine
			},
			{
				conditions: [{cond: ItemCountLessThan, value: 3}, {cond: ExceedsMaxLineLength, value: 0}],
				type: NoWrap
			},
			{
				conditions: [
					{cond: TotalItemLengthLessThan, value: 120},
					{cond: ExceedsMaxLineLength, value: 0}
				],
				type: NoWrap
			},
			{
				conditions: [{cond: ItemCountLargerThan, value: 4}],
				location: BeforeLast,
				type: OnePerLineAfterFirst
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				location: BeforeLast,
				type: FillLine
			}
		]
	})
	@:optional
	var opBoolChain:WrapRules;

	/**
		implements / extends chain wrapping rules
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: LineLengthLargerThan, value: 140}],
				type: FillLine,
				additionalIndent: 2
			},
			{
				conditions: [{cond: ItemCountLargerThan, value: 4}],
				type: FillLine,
				additionalIndent: 2
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: FillLine,
				additionalIndent: 2
			}
		]
	})
	@:optional
	var implementsExtends:WrapRules;

	/**
		chain wrapping rules for OpAdd / OpSub
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [
					{cond: LineLengthLargerThan, value: 160},
					{cond: AnyItemLengthLargerThan, value: 60}
				],
				location: BeforeLast,
				type: OnePerLineAfterFirst
			},
			{
				conditions: [{cond: LineLengthLargerThan, value: 160}],
				location: BeforeLast,
				type: FillLine
			},
			{
				conditions: [{cond: ItemCountLessThan, value: 3}, {cond: ExceedsMaxLineLength, value: 0}],
				type: NoWrap
			},
			{
				conditions: [
					{cond: TotalItemLengthLessThan, value: 120},
					{cond: ExceedsMaxLineLength, value: 0}
				],
				type: NoWrap
			},
			{
				conditions: [{cond: ItemCountLargerThan, value: 4}],
				location: BeforeLast,
				type: OnePerLineAfterFirst
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				location: BeforeLast,
				type: OnePerLineAfterFirst
			}
		]
	})
	@:optional
	var opAddSubChain:WrapRules;

	/**
		chain wrapping rules for OpAdd / OpSub
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: AnyItemLengthLessThan, value: 15}],
				type: FillLine
			},
			{
				conditions: [{cond: LineLengthLargerThan, value: 80},],
				type: OnePerLineAfterFirst
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: OnePerLineAfterFirst
			}
		]
	})
	@:optional
	var multiVar:WrapRules;

	/**
		chain wrapping rules for case patterns
	**/
	@:default({
		defaultWrap: NoWrap,
		rules: [
			{
				conditions: [{cond: ItemCountLargerThan, value: 2}],
				type: FillLine
			},
			{
				conditions: [{cond: ExceedsMaxLineLength, value: 1}],
				type: FillLine
			}
		]
	})
	@:optional
	var casePattern:WrapRules;
}

typedef WrapRules = {
	/**
		list of wrapping rules
		wrapping uses only the first rule whose conditions evaluates to true
	**/
	@:default([]) @:optional var rules:Array<WrapRule>;

	/**
		default wrapping type when no rule applies
	**/
	@:default(NoWrap) @:optional var defaultWrap:WrappingType;

	/**
		default wrapping location before / after last token
	**/
	@:default(AfterLast) @:optional var defaultLocation:WrappingLocation;

	/**
		adds indentation to all wrapped lines when applying defaultWrap
	**/
	@:default(0) @:optional var defaultAdditionalIndent:Int;
}

typedef WrapRule = {
	/**
		list of conditions
		wrapping selects a rule if all of its conditions evaluate to true
	**/
	var conditions:Array<WrapCondition>;

	/**
		wrapping type
	**/
	var type:WrappingType;

	/**
		default wrapping location before / after last token
	**/
	@:default(AfterLast) @:optional var location:WrappingLocation;

	/**
		adds indentation to all wrapped lines
	**/
	@:default(0) @:optional var additionalIndent:Int;
}

enum abstract WrappingType(String) {
	/**
		every item in a separate line, including first item
	**/
	var OnePerLine = "onePerLine";

	/**
		every item in a separate line, except first item
	**/
	var OnePerLineAfterFirst = "onePerLineAfterFirst";

	/**
		put an equal amount of items per line - not yet implemented
	**/
	var EqualNumber = "equalNumber";

	/**
		fill each line until maxLineLength - does not start with a newline
	**/
	var FillLine = "fillLine";

	/**
		fill each line until maxLineLength, starts with a newline before first item
	**/
	var FillLineWithLeadingBreak = "fillLineWithLeadingBreak";

	/**
		do not wrap items
	**/
	var NoWrap = "noWrap";

	/**
		use wrapping information from source
	**/
	var Keep = "keep";
}

enum abstract WrappingLocation(String) {
	var BeforeLast = "beforeLast";
	var AfterLast = "afterLast";
}

typedef WrapCondition = {
	var cond:WrapConditionType;
	@:default(1) @:optional var value:Int;
}

enum abstract WrapConditionType(String) {
	/**
		condition matches if item count is larger than or equal n items
	**/
	var ItemCountLargerThan = "itemCount >= n";

	/**
		condition matches if item count is less than or equal n items
	**/
	var ItemCountLessThan = "itemCount <= n";

	/**
		condition matches if max item length is larger than or equal n characters
	**/
	var AnyItemLengthLargerThan = "anyItemLength >= n";

	/**
		condition matches if max item length is less than or equal n characters
	**/
	var AllItemLengthsLessThan = "allItemLengths <= n";

	/**
		condition matches if min item length is larger than or equal n characters
	**/
	var AllItemLengthsLargerThan = "allItemLengths >= n";

	/**
		condition matches if min item length is less than or equal n characters
	**/
	var AnyItemLengthLessThan = "anyItemLength <= n";

	/**
		condition matches if total length of all wrapable items is larger than or equal n characters
	**/
	var TotalItemLengthLargerThan = "totalItemLength >= n";

	/**
		condition matches if total length of all wrapable items is less than or equal n characters
	**/
	var TotalItemLengthLessThan = "totalItemLength <= n";

	/**
		condition matches lines larger than or equal n characters
	**/
	var LineLengthLargerThan = "lineLength >= n";

	/**
		condition matches lines less than or equal n characters
	**/
	var LineLengthLessThan = "lineLength <= n";

	/**
		condition value = 1 matches if line contains a multiline token (string literal, block comment)
		value = 0 matches if no multiline token is present
	**/
	var HasMultiLineItems = "hasMultilineItems";

	/**
		condition value = 1 matches if unwrapped line exceeds maxLineLength
		value = 0 matches unwrapped lines not exceeding maxLineLength
	**/
	var ExceedsMaxLineLength = "exceedsMaxLineLength";

	/**
		condition value = 1 matches if all items have identical item length
		value = 0 matches any two items having different item lengths
	**/
	var EqualItemLengths = "equalItemLengths";
}

typedef ArrayWrapping = {
	@:default(30) @:optional var maxInlineAtLength:Int;
	@:default(30) @:optional var maxItemLength:Int;
	@:default(2) @:optional var maxOneLineItems:Int;
	@:default(60) @:optional var totalItemLengthOneLine:Int;
}

enum abstract ArrayMatrixWrap(String) {
	var NoMatrixWrap = "noMatrixWrap";
	var MatrixWrapNoAlign = "matrixWrapNoAlign";
	var MatrixWrapWithAlign = "matrixWrapWithAlign";
}
