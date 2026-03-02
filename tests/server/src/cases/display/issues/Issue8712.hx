package cases.display.issues;

class Issue8712 extends DisplayTestCase {
	/**
		abstract Foo(Int) {
			function foo() {
				ab{-2-}stract;
				ab{-3-}stract.bar();
			}

			function bar() return th{-1-}is;
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.isTrue(result.result.item.kind == (cast "Local" : Dynamic));
		Assert.equals("this", result.result.item.args.name);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		result = parseHover();
		Assert.isTrue(result.result.item.kind == (cast "Literal" : Dynamic));
		Assert.equals("abstract", result.result.item.args.name);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		result = parseHover();
		Assert.isTrue(result.result.item.kind == (cast "Literal" : Dynamic));
		Assert.equals("abstract", result.result.item.args.name);
	}
}
