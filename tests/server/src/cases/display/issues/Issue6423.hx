package cases.display.issues;

class Issue6423 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}

			macro function foo(expr:haxe.macro.Expr, field:String) {
				switch (expr) {
					case macro $expr.{-1-}$fie{-2-}ld{-3-}:
						expr;
				}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);
	}
}
