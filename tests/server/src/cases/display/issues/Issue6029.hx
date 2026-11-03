package cases.display.issues;

class Issue6029 extends DisplayTestCase {
	/**
		typedef A = {}
		typedef B = {}

		typedef C = {
			>{-1-}A,
			>{-2-}B,
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("A", result.item.type.args.path.typeName);

		result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.equals("B", result.item.type.args.path.typeName);
	}
}
