package cases.display.issues;

class Issue9133 extends DisplayTestCase {
	/**
		class Main {
		static function main() {
			var i = 1;
			var s = "";

			var map:Map<Int, Int> = [
				{-1-}
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		var items = result.result.items;
		var i1 = -1;
		var i2 = -1;
		for (idx in 0...items.length) {
			if (items[idx].kind == Local && items[idx].args.name == "i") i1 = idx;
			if (items[idx].kind == Local && items[idx].args.name == "s") i2 = idx;
		}
		Assert.isTrue(i1 != -1);
		Assert.isTrue(i1 < i2);
	}

	/**
		class Main {
		static function main() {
			var i = 1;
			var s = "";

			var map:Map<Int, Int> = [
				i => 1,
				{-1-}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		var items = result.result.items;
		var i1 = -1;
		var i2 = -1;
		for (idx in 0...items.length) {
			if (items[idx].kind == Local && items[idx].args.name == "i") i1 = idx;
			if (items[idx].kind == Local && items[idx].args.name == "s") i2 = idx;
		}
		Assert.isTrue(i1 != -1);
		Assert.isTrue(i1 < i2);
	}

	/**
		class Main {
		static function main() {
			var i = 0;
			{-1-}// comment
	**/
	function test3(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		var items = result.result.items;
		var i1 = -1;
		for (idx in 0...items.length) {
			if (items[idx].kind == Local && items[idx].args.name == "i") i1 = idx;
		}
		Assert.isTrue(i1 != -1);
	}
}
