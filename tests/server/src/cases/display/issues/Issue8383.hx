package cases.display.issues;

class Issue8383 extends DisplayTestCase {
	/**
		class Main {
			static var field(never,default):Int;
			static function main() {
				fi{-1-}eld = 10;
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("Int", result.result.item.type.args.path.typeName);
	}
}
