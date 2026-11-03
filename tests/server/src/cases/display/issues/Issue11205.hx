package cases.display.issues;

class Issue11205 extends DisplayTestCase {
	/**
		typedef Foo = {
			var {-1-}bar{-2-}:{
				var {-3-}value{-4-}:Int;
			};
		}
		function main() {
			final foo:Foo = cast null;
			foo?.b{-5-}ar?.v{-6-}alue;
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(5)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(6)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(3, 4), locs[0].range);

		Assert.equals("Null", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(6)}).item.type.args.path.typeName);
	}
}
