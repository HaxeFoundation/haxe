package cases.issues;

class Issue9029 extends TestCase {
	function testIssue9029_analyzer_preventPurityOnOverridden(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9029/Main.hx"));
		vfs.putContent("Game.hx", getTemplate("issues/Issue9029/Game.hx"));
		vfs.putContent("Screen.hx", getTemplate("issues/Issue9029/Screen.hx"));
		var args = ["-main", "Main", "-D", "analyzer-optimize", "--interp"];
		runHaxe(args);
		vfs.putContent("Game.hx", getTemplate("issues/Issue9029/Game.hx.modified"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Game.hx")});
		runHaxe(args);
		assertSuccess();
	}
}