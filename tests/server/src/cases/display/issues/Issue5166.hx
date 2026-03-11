package cases.display.issues;

class Issue5166 extends DisplayTestCase {
	/**
		enum abstract E(Int) {
			var {-2-}A{-1-} = 5;
		}

	**/
	function test(_) {
		Assert.equals("E", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.args.path.typeName);

		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(2, 1), locs[0].range);
	}
}
