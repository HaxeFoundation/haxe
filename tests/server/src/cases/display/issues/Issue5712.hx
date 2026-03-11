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
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(4)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		Assert.equals("Float", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)}).item.type.args.path.typeName);

		Assert.equals("Float", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(4)}).item.type.args.path.typeName);
	}
}
