package cases.issues;

class Issue11203 extends TestCase {
	function testClass(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11203/MainClass.hx"));
		var args = ["Main", "--interp"];
		runHaxe(args);
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}).length);
	}

	function testAbstract(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11203/MainAbstract.hx"));
		var args = ["Main", "--interp"];
		runHaxe(args);
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}).length);
	}
}
