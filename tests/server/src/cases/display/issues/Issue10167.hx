package cases.display.issues;

class Issue10167 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				[] is {-1-}Ar{-2-}ray{-3-};
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(2)
		});
		Assert.same(range(1, 3), result.range);
	}
}