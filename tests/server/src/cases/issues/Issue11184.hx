package cases.issues;

class Issue11184 extends TestCase {
	function testDiagnostics(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11184/Main.hx"));
		var args = ["-main", "Main", "-js", "bin/test.js"];

		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));
		final diagnostics = haxe.Json.parse(lastResult.stderr)[0].diagnostics;
		Assert.equals(diagnostics[0].args, "Cannot use Void as value");

		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	}

	function testWithoutCacheFromDisplay(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11184/Main.hx"));
		var args = ["-main", "Main", "-js", "bin/test.js"];
		runHaxeJson([], ServerMethods.Configure, {populateCacheFromDisplay: false});
		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));
		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	}
}
