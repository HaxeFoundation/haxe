package cases.issues;

import utest.Assert;

class Issue12224 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue12224/Main.hx"));
		var args = [
			"-main",
			"Main",
			"--js",
			"test.js",
			"-D",
			"analyzer-optimize",
			"--cmd",
			"node test.js"
		];

		runHaxe(args);
		assertSuccess();
		var initialContents = vfs.getContent("test.js");

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});

		runHaxe(args);
		assertSuccess();
		var contents = vfs.getContent("test.js");
		Assert.equals(initialContents, contents);
	}
}
