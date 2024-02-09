package cases.display.issues;

class Issue9790 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				name();
				name(0);
			}

			overload static function name() {
				trace("Hello");
			}

			overload static function {-1-}na{-2-}me{-3-}(i:Int) {
				trace("Hello " + i);
			}
		}
	**/
	function test(_) {
		var args = ["-main", "Main", "--no-output", "--jvm", "no.jar"];
		runHaxe(args);
		runHaxeJson(args, DisplayMethods.Hover, {file: file, offset: offset(2)});
		var result = parseHover();
		Assert.same(range(1, 3), result.result.range);
		Assert.pass();
	}
}
