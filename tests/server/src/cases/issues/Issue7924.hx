package cases.issues;

class Issue7924 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue7924/Main.hx"));
		vfs.putContent("ReturnTypeHintOption.hx", getTemplate("issues/Issue7924/ReturnTypeHintOption.hx"));
		var args = ["-main", "Main"];

		var diagnostics = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});
		Assert.equals(0, diagnostics.length);
		assertSuccess();
	}
}
