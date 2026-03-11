package cases.display.issues;

class Issue7020 extends DisplayTestCase {
	/**
		import String as {-2-}ExprAc{-4-}cess{-3-};

		class Main {
			public static function main() {
				var access:ExprA{-1-}ccess;
			}
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(4)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(2, 3), locs[0].range);

		Assert.equals("String", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)}).item.type.args.path.typeName);
	}
}
