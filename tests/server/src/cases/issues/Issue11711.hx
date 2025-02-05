package cases.issues;

import haxe.display.Diagnostic;

class Issue11711 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11711/Main.hx"));
		var args = ["-main", "Main", "--js", "no.js", "--no-output"];
		runHaxe(args);
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(1, res.length);
			var diag = res[0];
			Assert.equals(1, diag.diagnostics.length);
			var diag = diag.diagnostics[0];
			Assert.equals(Warning, diag.severity);
			Assert.equals("WInfo", diag.code);
			Assert.equals("Int", diag.args);
		});

		vfs.putContent("Main.hx", getTemplate("issues/Issue11711/Main1.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			trace(res);
		});
	}
}
