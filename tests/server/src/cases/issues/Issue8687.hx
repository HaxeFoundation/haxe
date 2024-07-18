package cases.issues;

class Issue8687 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue8687/Main.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(1, res.length);
			Assert.equals(1, res[0].diagnostics.length);
			Assert.equals(res[0].diagnostics[0].args, "Invalid version string \"foo\". Should follow SemVer.");
		});
	}
}
