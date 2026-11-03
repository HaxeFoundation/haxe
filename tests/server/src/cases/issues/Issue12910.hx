package cases.issues;

class Issue12910 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue12910/Main.hx"));
		vfs.putContent("Color.hx", getTemplate("issues/Issue12910/Color.hx"));
		var args = ["-main", "Main"];

		var diagnostics = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});
		Assert.equals(0, diagnostics.length);
		assertSuccess();
	}
}
