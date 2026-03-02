package cases.display.issues;

class Issue6399 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {}

			macro function foo({-1-}name{-2-}:String, {-3-}struct{-4-}:haxe.macro.Expr, {-5-}defaults{-6-}:haxe.macro.Expr) {
				return macro {
					if ($str{-7-}uct.$n{-8-}ame == null) $str{-9-}uct.$n{-10-}ame = $defa{-11-}ults.$n{-12-}ame;
				}
			}
		}
	**/
	function test(_) {
		for (i in [8, 10, 12]) {
			runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(i)});
			var locs = parseGotoDefintion().result;
			Assert.isTrue(locs != null && locs.length > 0);
			Assert.same(range(1, 2), locs[0].range);

			runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(i)});
			Assert.equals("String", parseHover().result.item.type.args.path.typeName);
		}

		for (i in [7, 9]) {
			runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(i)});
			var locs = parseGotoDefintion().result;
			Assert.isTrue(locs != null && locs.length > 0);
			Assert.same(range(3, 4), locs[0].range);

			runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(i)});
			Assert.equals("Expr", parseHover().result.item.type.args.path.typeName);
		}

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(11)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(5, 6), locs[0].range);
	}
}
