package cases.display.issues;

class Issue7098 extends DisplayTestCase {
	/**
		import issue7098.Bar;
		class Main {
			public static function main() {
				Bar.foo(Va{-1-}lue);
			}
		}
	**/
	function test(_) {
		vfs.putContent("issue7098/Bar.hx", "package issue7098;\n\nenum abstract Foo(Int) {\n\tvar Value = 0;\n}\n\nclass Bar {\n\tpublic static function foo(f:Foo) {}\n}");
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		// The definition is in issue7098/Bar.hx
		Assert.isTrue(locs[0].file.toString().indexOf("Bar.hx") != -1);
	}
}
