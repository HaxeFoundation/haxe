package cases.issues;

class Issue7197 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue7197/Main.hx"));
		vfs.putContent("A.hx", getTemplate("issues/Issue7197/A.hx"));
		var args = ["-main", "Main"];

		var diagnostics = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});
		Assert.equals(0, diagnostics.length);
		assertSuccess();
	}
}
