package cases.display.issues;

class Issue5166 extends DisplayTestCase {
	/**
		enum abstract E(Int) {
			var {-2-}A{-1-} = 5;
		}

	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("E", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(2, 1), locs[0].range);
	}
}
