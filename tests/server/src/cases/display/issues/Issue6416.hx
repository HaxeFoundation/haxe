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
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(4)});
		locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("String", parseHover().result.item.type.args.path.typeName);
	}
}
