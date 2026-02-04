package cases.issues;

class Issue12554 extends TestCase {
	#if !disable_hxb_cache
	function test(_) {
		var mainTpl = storeAndParseTemplate("issues/Issue12554/Main.hx", "Main.hx");
		var fooTpl = storeAndParseTemplate("issues/Issue12554/Foo.hx", "Foo.hx");

		var args = ["-main", "Main", "--no-output"];
		runHaxe(args);
		assertSuccess();

		var sig = "";
		runHaxeJsonCb(args, ServerMethods.Contexts, null, res -> sig = res[0].signature);
		Assert.notEquals(sig, "");

		runHaxeJsonCb(args, ServerMethods.Module, {
			path: "Main",
			signature: sig
		}, res -> Assert.equals("Good", res.cacheState));

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Foo.hx")});

		runHaxeJsonCb(args, DisplayMethods.Hover, {
			file: new FsPath("Main.hx"),
			offset: mainTpl.markers[1]
		}, res -> {});
		assertSuccess();

		runHaxeJsonCb(args, ServerMethods.Module, {
			path: "Main",
			signature: sig
		}, res -> Assert.equals("Good", res.cacheState));
	}
	#end
}
