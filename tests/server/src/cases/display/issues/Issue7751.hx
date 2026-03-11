package cases.display.issues;

class Issue7751 extends DisplayTestCase {
	/**
		extern class Foo {
			function new():V{-1-}oid;
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("Void", result.item.type.args.path.typeName);
	}
}
