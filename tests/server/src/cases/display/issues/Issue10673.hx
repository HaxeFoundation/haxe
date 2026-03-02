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
		vfs.putContent("issue10673/Macro.hx", "package issue10673;\n\nclass Macro {\n\tpublic static function build() {\n\t\tvar fields = haxe.macro.Context.getBuildFields();\n\t\tfor (field in fields) {\n\t\t\tswitch field.kind {\n\t\t\t\tcase FVar(_, e):\n\t\t\t\t\tfield.kind = FVar(TPath({pack: [\"std\"], name: \"StdTypes\", sub: \"Int\"}), e);\n\t\t\t\tcase _:\n\t\t\t}\n\t\t}\n\t\treturn fields;\n\t}\n}");
		var defRange = range(10, 11);
		for (i in 1...10) {
			runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(i)});
			var result = parseHover();
			Assert.isTrue(result.result != null);
			Assert.isTrue(result.result.item.kind == (cast "Local" : Dynamic));
			Assert.equals("rhs_______", result.result.item.args.name);
		}
	}
}
