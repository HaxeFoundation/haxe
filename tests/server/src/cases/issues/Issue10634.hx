package cases.issues;

class Issue10634 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue10634/Main.hx"));
		var args = ["-main", "Main"];

		var diagnostics = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});
		Assert.equals(0, diagnostics.length);
		assertSuccess();
	}
}
