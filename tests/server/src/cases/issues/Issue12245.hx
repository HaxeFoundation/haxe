package cases.issues;

import haxe.io.Path;

class Issue12245 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", "function main() {\n\ttrace(haxe.io.Bytes.alloc(0));\n}");
		var args = ["-main", "Main", "--js", "bin/main.js", "--no-output"];
		runHaxe(args);
		assertSuccess();

		var std = Path.removeTrailingSlashes(utils.macro.BuildHub.getStd());
		var bytes = new FsPath('$std/js/_std/haxe/io/Bytes.hx');

		// Used to loop forever spawning core-api typing contexts, because the
		// display file was registered under its file-derived dotpath
		// js._std.haxe.io.Bytes instead of haxe.io.Bytes
		final res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: bytes});
		assertSuccess();
		// Only assert on errors: warning-level diagnostics (e.g. unused vars)
		// legitimately occur in the real std file
		for (r in res)
			Assert.equals(0, r.diagnostics.filter(d -> d.severity == Error).length);

		// Used to fail with "Invalid commandline class : js._std.haxe.io.Bytes
		// should be haxe.io.Bytes"
		runHaxeJson(args, DisplayMethods.Hover, {file: bytes, offset: 0});
		assertSuccess();
	}

	// "Reload window" scenario: the diagnostics request on the _std file is
	// the first thing the fresh server sees, without a prior compilation
	function testWithoutPriorCompile(_) {
		vfs.putContent("Main.hx", "function main() {\n\ttrace(haxe.io.Bytes.alloc(0));\n}");
		var args = ["-main", "Main", "--js", "bin/main.js", "--no-output"];

		var std = Path.removeTrailingSlashes(utils.macro.BuildHub.getStd());
		var bytes = new FsPath('$std/js/_std/haxe/io/Bytes.hx');

		final res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: bytes});
		assertSuccess();
		for (r in res)
			Assert.equals(0, r.diagnostics.filter(d -> d.severity == Error).length);
	}
}
