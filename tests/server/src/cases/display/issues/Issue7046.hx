package cases.display.issues;

class Issue7046 extends DisplayTestCase {
	/**
		import Ar{-1-}ray;
		using Ar{-2-}ray;
	**/
	function test(_) {
		Assert.equals("Array", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.args.path.typeName);

		Assert.equals("Array", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)}).item.type.args.path.typeName);
	}
}
