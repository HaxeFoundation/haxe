package cases.issues;

import utest.Assert;

class Issue11311 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11311/Main.hx"));
		var args = [
			"-main",
			"Main",
			"--js",
			"test.js",
			"-D",
			"dce=full",
			"-D",
			"analyzer-optimize",
			"--cmd",
			"node test.js"
		];

		runHaxe(args);
		assertSuccess();

		vfs.putContent("Main.hx", getTemplate("issues/Issue11311/Main.hx").replace("// ", ""));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {});

		runHaxe(args);
		assertSuccess();
		Assert.isTrue(vfs.getContent("test.js").contains("iterator: function() {"));
	}
}
