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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(result.item.kind == (cast "Local" : Dynamic));
		Assert.equals("this", result.item.args.name);

		result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(result.item.kind == (cast "Literal" : Dynamic));
		Assert.equals("abstract", result.item.args.name);

		result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.isTrue(result.item.kind == (cast "Literal" : Dynamic));
		Assert.equals("abstract", result.item.args.name);
	}
}
