package cases.display.issues;

class Issue9047 extends DisplayTestCase {
	/**
		interface Main { var field(never,s{-1-}et):Int; }
	**/
	function test(_) {
		var args = ["Main", "-js", "main.js"];
		function parseGotoDefintion():GotoDefinitionResult {
			return haxe.Json.parse(lastResult.stderr).result;
		}
		runHaxeJson(args, DisplayMethods.FindReferences, {file: file, offset: offset(1), contents: source});
		Assert.same([], parseGotoDefintion().result);
		runHaxeJson(args, DisplayMethods.FindReferences, {file: file, offset: offset(1), contents: source});
		Assert.same([], parseGotoDefintion().result);
	}
}