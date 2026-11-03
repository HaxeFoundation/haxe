package cases.display.issues;

class Issue10638 extends DisplayTestCase {
	/**
		var x = {-1-}"f{-2-}oo"{-3-};

		class Main {
			static function main() {}
		}
	**/
	function test(_) {
		runHaxe(["--main", "Main"]);
		var result = runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(2)
		});
		Assert.same(range(1, 3), result.range);
	}

}