package cases.display.issues;

class Issue9319 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				try {} catch(e{-1-}) {}
			}
		}
	**/
	function testCatch_noTypeHint(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("Exception", result.result.item.type.args.path.typeName);
		Assert.equals("haxe", result.result.item.type.args.path.pack[0]);
	}
}
