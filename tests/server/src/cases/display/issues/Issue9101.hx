package cases.display.issues;

class Issue9101 extends DisplayTestCase {
	/**
		typedef T = {
		?{-1-}t{-2-}e{-3-}st:Int
		}
	**/
	function testCatch_noTypeHint(_) {
		for (n in [1, 2, 3]) {
			var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(n)});
			Assert.equals("Null", result.item.type.args.path.typeName);
		}
	}
}
