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
		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(2)
		});
		var result = parseHover();
		Assert.same(range(1, 3), result.result.range);
	}

}