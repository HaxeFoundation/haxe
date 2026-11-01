package cases.issues;

class Issue11184 extends TestCase {
	function testDiagnostics(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11184/Main.hx"));
		var args = ["-main", "Main", "-js", "bin/test.js"];

		final res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});
		Assert.equals(1, res.length);
		Assert.equals(1, res[0].diagnostics.length);
		Assert.equals(res[0].diagnostics[0].args, "Cannot use Void as value");

		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	}
}
