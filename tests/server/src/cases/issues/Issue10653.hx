package cases.issues;

class Issue10653 extends TestCase {
	function test(_) {
		vfs.putContent("Test.hx", getTemplate("issues/Issue10653/Test.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue10653/MainBefore.hx"));
		var args = ["-main", "Main", "--js", "no.js", "--no-output"];
		runHaxe(args);
		vfs.putContent("Main.hx", getTemplate("issues/Issue10653/MainAfter.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));
		Assert.isTrue(lastResult.stderr.length == 2);
	}
}
