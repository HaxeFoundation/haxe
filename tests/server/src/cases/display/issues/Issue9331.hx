package cases.display.issues;

class Issue9331 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				{-1-}Main.fu{-2-}nc{-3-}();
			}

			static macro function func() {
				return macro 1;
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
