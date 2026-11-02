package cases.display.issues;

class Issue11067 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var str = "hello";
				trace(st{-1-}r);
			}
		}
	**/
	function test(_) {
		var args = ["-main", "Main", "--interp", "--next", "-main", "Main", "--interp"];
		var result = runHaxeJson(args, DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.notNull(result);
		Assert.equals("String", result.item.type.args.path.typeName);
	}
}
