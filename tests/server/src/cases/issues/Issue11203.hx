package cases.issues;

class Issue11203 extends TestCase {
	function testClass(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11203/MainClass.hx"));
		var args = ["Main", "--interp"];
		runHaxe(args);
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(0, res.length);
		});
	}

	function testAbstract(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11203/MainAbstract.hx"));
		var args = ["Main", "--interp"];
		runHaxe(args);
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(0, res.length);
		});
	}
}
