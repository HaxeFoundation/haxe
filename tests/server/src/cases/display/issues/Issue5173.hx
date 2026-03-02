package cases.display.issues;

class Issue5173 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var map = new haxe.DynamicAccess();
				for ({-3-}ke{-1-}y{-4-} in map.keys()) {
					{-5-}ke{-2-}y{-6-};
				}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(3, 4), locs[0].range);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(3, 4), locs[0].range);
	}
}
