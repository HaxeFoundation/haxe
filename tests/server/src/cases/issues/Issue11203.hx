package cases.issues;

class Issue11203 extends TestCase {
	function testClass(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11203/MainClass.hx"));
		var args = ["Main", "--interp"];
		runHaxe(args);
		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));

		var diag = parseDiagnostics();
		Assert.isTrue(diag.length == 0);
	}

	function testAbstract(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11203/MainAbstract.hx"));
		var args = ["Main", "--interp"];
		runHaxe(args);
		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));

		var diag = parseDiagnostics();
		Assert.isTrue(diag.length == 0);
	}
}
