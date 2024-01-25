package cases.issues;

class Issue10646 extends TestCase {
	function test(_) {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["-main", "HelloWorld", "--neko", "test.n"];
		runHaxe(args);
		var nekoCtx = null;
		runHaxeJsonCb([], ServerMethods.Contexts, null, function(ctxs) {
			for (ctx in ctxs) {
				if (ctx.desc == "after_init_macros") {
					nekoCtx = ctx;
				}
			}
		});
		Assert.notNull(nekoCtx);
		runHaxeJsonCb([], ServerMethods.ContextMemory, {signature: nekoCtx.signature}, function(mem) {
			Assert.isNull(mem.leaks);
		});
	}
}
