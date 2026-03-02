package cases.display.issues;

class Issue6442 extends DisplayTestCase {
	/**
		extern class Foo {
			function {-1-}b{-2-}ar{-3-}():Void;
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));
	}
}
