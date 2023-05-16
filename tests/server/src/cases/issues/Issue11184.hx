package cases.issues;

class Issue11184 extends TestCase {
	function testDiagnostics(_) {
		final content = getTemplate("issues/Issue11184/Main.hx");
		final transform = Marker.extractMarkers(content);

		vfs.putContent("Main.hx", transform.source);
		var args = ["-main", "Main", "-js", "bin/test.js"];

		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));
		final diagnostics = haxe.Json.parse(lastResult.stderr)[0].diagnostics;
		Assert.equals(diagnostics[0].args, "Cannot use Void as value");

		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	}

	function testFindReferences(_) {
		final content = getTemplate("issues/Issue11184/Main.hx");
		final transform = Marker.extractMarkers(content);

		vfs.putContent("Main.hx", transform.source);
		var args = ["-main", "Main", "-js", "bin/test.js"];

		runHaxeJson(args, DisplayMethods.FindReferences, {
			file: new FsPath("Main.hx"),
			offset: transform.markers.get(1),
			contents: transform.source
		});
		final errors = haxe.Json.parse(lastResult.stderr).error?.data;
		Assert.equals(errors[0].message, "Cannot use Void as value");

		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	}

	function testWithoutCacheFromDisplay(_) {
		final content = getTemplate("issues/Issue11184/Main.hx");
		final transform = Marker.extractMarkers(content);

		vfs.putContent("Main.hx", transform.source);
		var args = ["-main", "Main", "-js", "bin/test.js"];
		runHaxeJson([], ServerMethods.Configure, {populateCacheFromDisplay: false});
		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));
		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
		runHaxe(args);
		Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	}
}
