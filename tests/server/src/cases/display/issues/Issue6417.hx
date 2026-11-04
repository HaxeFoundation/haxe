package cases.display.issues;

class Issue6417 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}

			macro function foo({-1-}body{-2-}:haxe.macro.Expr) {
				macro function() $bo{-3-}dy;
			}
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		Assert.equals("Expr", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)}).item.type.args.path.typeName);
	}
}
