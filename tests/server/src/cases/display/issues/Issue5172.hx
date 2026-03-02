package cases.display.issues;

class Issue5172 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				for ({-3-}unique_identifier_5172{-1-} in 0...10) {
					{-4-}unique_identifier_5172{-2-};
				}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("Int", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("Int", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(3, 1), locs[0].range);
	}
}
