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
		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.args.path.typeName);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.args.path.typeName);

		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(3, 4), locs[0].range);

		locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(3, 4), locs[0].range);
	}
}
