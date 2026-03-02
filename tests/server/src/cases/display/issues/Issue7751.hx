package cases.display.issues;

class Issue7751 extends DisplayTestCase {
	/**
		extern class Foo {
			function new():V{-1-}oid;
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("Void", result.result.item.type.args.path.typeName);
	}
}
