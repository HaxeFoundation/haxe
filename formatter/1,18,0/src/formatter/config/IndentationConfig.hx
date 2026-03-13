package formatter.config;

typedef IndentationConfig = {
	/**
		only applies to non inlined conditionals
		"fixedZero" = all conditional statements should start in column 1
		"fixedZeroIncrease" = indentation for conditionals starts at 0 and increases for every level
		"fixedZeroIncreaseBlocks" = same as "fixedZeroIncrease" but increases only inside blocks, outside it's "fixedZero"
		"aligned" = conditional statements share indentation of surrounding code
		"alignedNestedIncrease" = conditionals align with surrounding code, but will increase indent when nested
		"alignedIncrease" = same as "aligned" but will increase indent by +1 for enclosed code
		"alignedDecrease" = same as "aligned" but will decrease indent by -1 for enclosed code
	**/
	@:default(Aligned) @:optional var conditionalPolicy:ConditionalIndentationPolicy;

	/**
		use "tab", " ", "  ", "   ", "    ", etc. to define which character to use
	**/
	@:default("tab") @:optional var character:String;

	/**
		if `character` is set to "tab", formatter uses `tabWidth` to calculate absolute line length
	**/
	@:default(4) @:optional var tabWidth:Int;

	/**
		adds trailing whitespace to empty lines by copying indentation from preceeding line
	**/
	@:default(false) @:optional var trailingWhitespace:Bool;

	@:default(true) @:optional var indentObjectLiteral:Bool;

	/**
		indent complex value expressions:
			(true)						(false)
		var a = if (true)			var a = if (true)
				10;						10;
			else					else
				20;			vs.			20;
		return if (true)			return if (true)
				10;						10;
			else					else
				20;						20;
	**/
	@:default(false) @:optional var indentComplexValueExpressions:Bool;

	/**
		indent case / default labels (true) or keep them on same level as switch (false)
	**/
	@:default(true) @:optional var indentCaseLabels:Bool;
}

enum abstract ConditionalIndentationPolicy(String) {
	var FixedZero = "fixedZero";
	var FixedZeroIncrease = "fixedZeroIncrease";
	var FixedZeroIncreaseBlocks = "fixedZeroIncreaseBlocks";
	var Aligned = "aligned";
	var AlignedNestedIncrease = "alignedNestedIncrease";
	var AlignedIncrease = "alignedIncrease";
	var AlignedDecrease = "alignedDecrease";
}
