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
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.equals("Expr", parseHover().result.item.type.args.path.typeName);
	}
}
