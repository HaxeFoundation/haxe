package cases.display.issues;

class Issue6943 extends DisplayTestCase {
	/**
		class Main {
		public static function main() {}

		function foo(?{-1-}te{-2-}st{-3-}:Int) {}
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(2)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 3), locs[0].range);
	}
}
