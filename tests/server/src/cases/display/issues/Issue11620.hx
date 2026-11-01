package cases.display.issues;

class Issue11620 extends DisplayTestCase {
	/**
		import issue11620.Foo.Bar;

		function main() {
			Bar.bar();
		}
	**/
	function test(_) {
		vfs.putContent("issue11620/Foo.hx", "package issue11620;\n\nclass Foo {\n    public static function foo() {}\n}\n\nclass Bar {\n    public static function bar() {}\n}");
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(0, diags.length);
	}
}
