package cases.display.issues;

class Issue9463 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				function {-1-}na{-2-}me{-3-}() {
					trace("foo");
				}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.same(range(1, 3), result.range);
		Assert.pass();
	}
}
