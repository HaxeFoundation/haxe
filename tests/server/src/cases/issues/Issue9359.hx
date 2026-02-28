package cases.issues;

class Issue9359 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9359/Main.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertSuccess();
		assertHasPrint("stdout line");
		Assert.isTrue(lastResult.stderr.contains("stderr line"));
	}
}
