package cases.display.issues;

class Issue9047 extends DisplayTestCase {
	/**
		interface Main { var field(never,s{-1-}et):Int; }
	**/
	function test(_) {
		var args = ["Main", "-js", "main.js"];
		var result1 = runHaxeJson(args, DisplayMethods.FindReferences, {file: file, offset: offset(1), contents: source});
		Assert.same([], result1);
		var result2 = runHaxeJson(args, DisplayMethods.FindReferences, {file: file, offset: offset(1), contents: source});
		Assert.same([], result2);
	}
}