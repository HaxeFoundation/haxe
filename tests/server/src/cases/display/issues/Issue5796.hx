package cases.display.issues;

class Issue5796 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}
		}

		typedef Test = {
			@:gen{-1-}eric var f{-2-}oo:Int;
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.isTrue(result.result != null);
		Assert.equals("@:generic", result.result.item.args.name);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		result = parseHover();
		Assert.equals("Int", result.result.item.type.args.path.typeName);
	}
}
