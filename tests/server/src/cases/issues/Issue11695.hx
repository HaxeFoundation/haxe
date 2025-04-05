package cases.issues;

class Issue11695 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11695/Main.hx"));
		vfs.putContent("Macro.hx", getTemplate("issues/Issue11695/Macro1.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertHasPrint("Macro.hx:1: before");

		// Note: this is needed because modification time is currently checked with second precision
		Sys.sleep(1);

		vfs.putContent("Macro.hx", getTemplate("issues/Issue11695/Macro2.hx"));
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Macro.hx")}, res -> {
			Assert.equals(0, res.length);
		});

		runHaxe(args);
		assertHasPrint("Macro.hx:1: after");
	}

	function testLegacyDiagnostics(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11695/Main.hx"));
		vfs.putContent("Macro.hx", getTemplate("issues/Issue11695/Macro1.hx"));
		var args = ["-main", "Main", "--interp"];
		runHaxe(args);
		assertHasPrint("Macro.hx:1: before");

		// Note: this is needed because modification time is currently checked with second precision
		Sys.sleep(1);

		vfs.putContent("Macro.hx", getTemplate("issues/Issue11695/Macro2.hx"));
		runHaxe(args.concat(["--display", "Macro.hx@0@diagnostics"]));

		runHaxe(args);
		assertHasPrint("Macro.hx:1: after");
	}
}
