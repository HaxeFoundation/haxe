package cases.issues;

import haxe.display.Diagnostic;

class Issue10653 extends TestCase {
	function test(_) {
		vfs.putContent("Test.hx", getTemplate("issues/Issue10653/Test.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue10653/MainBefore.hx"));
		var args = ["-main", "Main", "--js", "no.js", "--no-output"];
		runHaxe(args);
		vfs.putContent("Main.hx", getTemplate("issues/Issue10653/MainAfter.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(0, res.length);
		});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(0, res.length);
		});
	}

	function testTypeHole(_) {
		vfs.putContent("Test.hx", getTemplate("issues/Issue10653/Test.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue10653/MainBefore.hx"));
		var args = ["-main", "Main", "--js", "no.js", "--no-output"];
		runHaxe(args);
		vfs.putContent("Main.hx", getTemplate("issues/Issue10653/MainAfterWrong.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(1, res.length);
			var arg:MissingFieldDiagnostics = cast res[0].diagnostics[0].args;
			Assert.equals("foo", arg.entries[0].fields[0].field.name);
		});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(1, res.length);
			var arg:MissingFieldDiagnostics = cast res[0].diagnostics[0].args;
			Assert.equals("foo", arg.entries[0].fields[0].field.name);
		});
	}
}
