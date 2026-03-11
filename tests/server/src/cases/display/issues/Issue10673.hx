package cases.display.issues;

class Issue10673 extends DisplayTestCase {
	/**
		@:build(issue10673.Macro.build())
		class Main {
			var a:Int;

			function func1():Void {
				var lhs_______ = 0;
				var {-10-}rhs_______{-11-} = 0;
				lhs_______ = rhs_____{-1-}__;
				lhs_______ = rhs_____{-2-}__;
				lhs_______ = rhs_____{-3-}__;
				lhs_______ = rhs_____{-4-}__;
				lhs_______ = rhs_____{-5-}__;
				lhs_______ = rhs_____{-6-}__;
				lhs_______ = rhs_____{-7-}__;
				lhs_______ = rhs_____{-8-}__;
				lhs_______ = rhs_____{-9-}__;
			}
		}
	**/
	function test(_) {
		vfs.putContent("issue10673/Macro.hx", getTemplate("display/issues/Issue10673/Macro.hx"));
		var defRange = range(10, 11);
		for (i in 1...10) {
			var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(i)});
			Assert.isTrue(result != null);
			Assert.isTrue(result.item.kind == (cast "Local" : Dynamic));
			Assert.equals("rhs_______", result.item.args.name);
		}
	}
}
