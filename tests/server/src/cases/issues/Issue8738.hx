package cases.issues;

class Issue8738 extends TestCase {
	function test(_) {
		vfs.putContent("Base.hx", getTemplate("issues/Issue8738/Base.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue8738/Main1.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		vfs.putContent("Main.hx", getTemplate("issues/Issue8738/Main2.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertErrorMessage("Cannot force inline-call to test because it is overridden");
		vfs.putContent("Main.hx", getTemplate("issues/Issue8738/Main3.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertSuccess();
	}
}