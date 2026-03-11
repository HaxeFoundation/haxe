package cases.display.issues;

class Issue6416 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}

			macro function foo(e:haxe.macro.Expr) {
				switch (e) {
					case macro $i{{-1-}f{-2-}oo{-3-}}:
						f{-4-}oo;
				}
			}
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);

		locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(4)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.args.path.typeName);
	}
}
