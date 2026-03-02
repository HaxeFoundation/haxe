package cases.display.issues;

class Issue5712 extends DisplayTestCase {
	/**
		typedef Struct = {
			{-1-}field{-2-}:Float
		}
		class Main {
			public static function main() {
				var s:Struct = { fi{-3-}eld: 0 };
				s.fi{-4-}eld;
			}
		}
	**/
	function testType1(_) {
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(4)});
		locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.equals("Float", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)});
		Assert.equals("Float", parseHover().result.item.type.args.path.typeName);
	}
}
