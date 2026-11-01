package cases.display.issues;

class Issue8073 extends DisplayTestCase {
	/**
		import haxe.ds.StringMap;

		class Main {
			static function main() {
				var v:{-1-}haxe.ds.Ve{-2-}ctor{-3-}<Int>;
				var v:{-4-}Stri{-5-}ngMap{-6-}<Int>;
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(2)
		});
		Assert.same(range(1, 3), result.range);

		var result = runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(5)
		});
		Assert.same(range(4, 6), result.range);
	}
}