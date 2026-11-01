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
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.args.path.typeName);
	}
}
