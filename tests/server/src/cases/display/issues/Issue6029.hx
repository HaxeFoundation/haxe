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
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("A", result.result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		result = parseHover();
		Assert.equals("B", result.result.item.type.args.path.typeName);
	}
}
