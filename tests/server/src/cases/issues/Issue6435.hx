package cases.issues;

import haxe.display.FsPath;

class Issue6435 extends TestCase {
	function test(_) {
		vfs.putContent("Main.js.hx", "class Main {\n\tstatic function main() {}\n}\n");
		final res = runHaxeJson(["-main", "Main", "-js", "bin/test.js", "--no-output"], DisplayMethods.Diagnostics,
			{file: new FsPath("Main.js.hx")});
		Assert.equals(0, res.length);
	}
}
