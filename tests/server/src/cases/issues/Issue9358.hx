package cases.issues;

class Issue9358 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9358/Main.hx"));
		vfs.putContent("StateHandler.hx", getTemplate("issues/Issue9358/StateHandler.hx"));
		var args = ["-cp", "src", "-m", "Main", "-hl", "hl.hl"];
		runHaxe(args);
		vfs.touchFile("Main.hx");
		runHaxe(args);
		assertSuccess();
	}
}
