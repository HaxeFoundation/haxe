package cases.display.issues;

class Issue6756 extends DisplayTestCase {
	/**
		abstract Result(String) {
			function f{-1-}oo() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("TFun", result.result.item.type.kind);
	}
}
