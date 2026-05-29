package cases.issues;

import haxe.display.Protocol;

class Issue12807 extends TestCase {
	static final BUILD_MACRO = "
		class BuildMacro {
			public static function build() return null;
		}
	";

	static final MAIN = "
		class Main {
			public static function main() Other;
		}
	";

	static final OTHER = "
		@:build(BuildMacro.build())
		class Other {}
	";

	final args = ["-main", "Main", "--js", "no.js", "--no-output", "-D", "testContextCleanup"];

	function testContextCleanup(_) {
		vfs.putContent("BuildMacro.macro.hx", BUILD_MACRO);
		vfs.putContent("Main.hx", MAIN);
		vfs.putContent("Other.hx", OTHER);
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		runHaxeJson([], Methods.Initialize, {staleContextMaxAge: 6});

		// Initial build
		runHaxe(args);
		assertSuccess();

		// Any request should trigger cleanup (will happen _after_ the request)
		var ctx = runHaxeJson([], ServerMethods.Contexts, null);
		Assert.equals(4, ctx.length);

		// Sleep past the stale-context threshold
		Sys.sleep(7);

		// Any request should trigger cleanup (will happen _after_ the request)
		runHaxeJson([], ServerMethods.Contexts, null);

		Sys.sleep(0.1);

		// Everything should be discarded
		var ctx = runHaxeJson([], ServerMethods.Contexts, null);
		Assert.equals(0, ctx.length);

		// Still builds fine (rebuilds all context)
		runHaxe(args);
		assertSuccess();
	}

	function testStaleMacroContext(_) {
		vfs.putContent("BuildMacro.macro.hx", BUILD_MACRO);
		vfs.putContent("Main.hx", MAIN);
		vfs.putContent("Other.hx", OTHER);
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		runHaxeJson([], Methods.Initialize, {staleContextMaxAge: 6});

		// Initial build
		runHaxe(args);
		assertSuccess();

		// Sleep halfway to the stale-context threshold
		Sys.sleep(3);

		// Run a request that refreshes the main context but not macro context
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("Empty.hx"), offset: 0});
		assertSuccess();

		// Sleep past the stale-context threshold
		Sys.sleep(4);

		// Any request should trigger cleanup (will happen _after_ the request)
		var ctx = runHaxeJson([], ServerMethods.Contexts, null);
		Assert.equals(4, ctx.length);

		Sys.sleep(0.1);

		// Both main and macro contexts should still be there
		// Note: both `load_core_class` will be discarded
		var ctx = runHaxeJson([], ServerMethods.Contexts, null);
		Assert.isTrue(Lambda.exists(ctx, c -> c.desc == "get_macro_context"));
		Assert.isTrue(Lambda.exists(ctx, c -> c.desc == "after_init_macros"));

		// Used to crash: main context depends on macro context that has been discarded
		// Compiler failure: Could not find dependency BuildMacro of Other in the cache
		runHaxe(args);
		assertSuccess();
	}
}
