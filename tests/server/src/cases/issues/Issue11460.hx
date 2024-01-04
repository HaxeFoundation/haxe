package cases.issues;

using StringTools;

class Issue11460 extends TestCase {
	function testClass(_) {
		var mainContent = getTemplate("issues/Issue11460/Main.hx");
		vfs.putContent("Main.hx", mainContent);
		vfs.putContent("C.hx", getTemplate("issues/Issue11460/C.hx"));
		var args = ["Main", "--interp"];

		// initial cache
		runHaxe(args);
		assertSilence();

		// touching Main doesn't do anything
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertSilence();

		// touching both doesn't do anything
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("C.hx")});
		runHaxe(args);
		assertSilence();

		// removing the inline is fine
		vfs.putContent("Main.hx", mainContent.replace("inline", ""));
		runHaxe(args);
		assertSilence();

		// adding it back is fine too because C is still cached
		vfs.putContent("Main.hx", mainContent);
		runHaxe(args);
		assertSilence();

		// removing the inline and changing C is still fine
		vfs.putContent("Main.hx", mainContent.replace("inline", ""));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("C.hx")});
		runHaxe(args);
		assertSilence();

		// but adding it now gives us the warning
		vfs.putContent("Main.hx", mainContent);
		runHaxe(args);
		utest.Assert.match(~/WInlineOptimizedField/, lastResult.stderr);
	}
}
