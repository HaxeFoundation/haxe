package cases.display.issues;

class Issue6756 extends DisplayTestCase {
	/**
		abstract Result(String) {
			function f{-1-}oo() {}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		final type = result.item.type;
		switch [type.kind, type.args] {
			case [TFun, args]:
				Assert.equals(0, args.args.length);
			case _:
				Assert.fail();
		}
	}
}
