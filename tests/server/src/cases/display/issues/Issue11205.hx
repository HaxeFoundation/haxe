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
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(5)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(6)});
		locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(3, 4), locs[0].range);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(6)});
		Assert.equals("Null", parseHover().result.item.type.args.path.typeName);
	}
}
