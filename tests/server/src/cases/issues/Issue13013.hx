package cases.issues;

class Issue13013 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue13013/Main.hx"));
		var args = ["-main", "Main", "--js", "out.js"];

		runHaxe(args);
		assertErrorMessage("Unknown identifier : foo");

		runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});

		runHaxe(args);
		Assert.isFalse(hasMessage("reusing Main"));
		assertErrorMessage("Unknown identifier : foo");
	}
}
