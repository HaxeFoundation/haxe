package cases.display.issues;

class Issue7148 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				function {-1-}local{-2-}() {}
				lo{-3-}cal;

				inline function {-4-}local{-5-}() {}
				lo{-6-}cal;
			}
		}
	**/
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(3)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(1, 2), locs[0].range);

		locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(6)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(4, 5), locs[0].range);
	}
}
