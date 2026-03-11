package cases.display.issues;

class Issue7046 extends DisplayTestCase {
	/**
		import Ar{-1-}ray;
		using Ar{-2-}ray;
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("Array", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("Array", parseHover().result.item.type.args.path.typeName);
	}
}
