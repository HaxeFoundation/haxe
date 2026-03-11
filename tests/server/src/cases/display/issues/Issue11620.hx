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
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(0, diags.length);
	}
}
