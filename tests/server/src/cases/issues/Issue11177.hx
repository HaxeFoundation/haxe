package cases.issues;

class Issue11177 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11177/Main.hx"));
		vfs.putContent("Buttons.hx", getTemplate("issues/Issue11177/Buttons.hx"));
		vfs.putContent("KeyCode.hx", getTemplate("issues/Issue11177/KeyCode.hx"));
		var args = ["-main", "Main", "--interp"];
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Buttons.hx")}).length);
		vfs.putContent("Main.hx", getTemplate("issues/Issue11177/Main2.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Buttons.hx")}).length);
	}
}
