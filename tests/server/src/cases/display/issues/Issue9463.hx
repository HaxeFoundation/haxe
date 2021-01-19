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
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		var result = parseHover();
		Assert.same(range(1, 3), result.result.range);
		Assert.pass();
	}
}
