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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(result != null);
		Assert.equals("@:generic", result.item.args.name);

		result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("Int", result.item.type.args.path.typeName);
	}
}
