package unit.issues;

import haxe.extern.EitherType;

private typedef PrepareRenameResult = EitherType<Range, EitherType<RangePlaceholder, DefaultBehaviour>>;

private typedef Range = {
	var start:Position;
	var end:Position;
}

private typedef RangePlaceholder = {
	var range:Range;
	var placeholder:String;
}

private typedef DefaultBehaviour = {
	var defaultBehavior:Bool;
}

private typedef Position = {
	var line:Int;
	var character:Int;
}

class Issue10712 extends unit.Test {
	function test() {
		var pos:Position = {line: 1, character: 10};
		var range:Range = {start: pos, end: pos};

		var result:PrepareRenameResult = {range: range, placeholder: "Haxe"};
		result = range;
		result = {defaultBehavior: true};

		test2({range: range, placeholder: "Haxe"});
		test2(range);
		test2({defaultBehavior: true});

		test2(cast {range: range, placeholder: "Haxe"});
		test2(range);
		test2(cast {defaultBehavior: true});

		utest.Assert.pass();
	}

	static function test2(param:PrepareRenameResult) {}
}
