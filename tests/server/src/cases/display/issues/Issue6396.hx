package cases.display.issues;

class Issue6396 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {}

			macro function foo() {
				var {-1-}name{-2-} = "name";
				return macro {
					$na{-3-}me;
				}
			}
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)}).item.type.args.path.typeName);
	}
}
