package cases.issues;

class Issue12554 extends TestCase {
	#if !disable_hxb_cache
	function test(_) {
		var mainTpl = storeAndParseTemplate("issues/Issue12554/Main.hx", "Main.hx");
		var fooTpl = storeAndParseTemplate("issues/Issue12554/Foo.hx", "Foo.hx");

		var args = ["-main", "Main", "--no-output"];
		runHaxe(args);
		assertSuccess();

		final sig = runHaxeJson(args, ServerMethods.Contexts, null)[0].signature;
		Assert.notEquals(sig, "");

		Assert.equals("Good", runHaxeJson(args, ServerMethods.Module, {
			path: "Main",
			signature: sig
		}).cacheState);

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Foo.hx")});

		runHaxeJson(args, DisplayMethods.Hover, {
			file: new FsPath("Main.hx"),
			offset: mainTpl.markers[1]
		});
		assertSuccess();

		Assert.equals("Good", runHaxeJson(args, ServerMethods.Module, {
			path: "Main",
			signature: sig
		}).cacheState);
	}
	#end
}
