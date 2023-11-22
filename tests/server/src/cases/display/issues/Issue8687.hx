package cases.display.issues;

class Issue8687 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue8687/Main.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));

		var diag = parseDiagnostics();
		Assert.equals("Invalid version string \"foo\". Should follow SemVer.", diag[0].args);
	}
}
