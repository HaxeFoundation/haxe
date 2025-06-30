package cases.issues;

import haxe.display.Diagnostic;
import utest.Async;

class Issue11995 extends TestCase {
	@:async
	@:variant("WithInvalidation", true)
	@:variant("WithoutInvalidation", false)
	function test(async:Async, invalidate:Bool) {
		var content = getTemplate("issues/Issue11995/Main.hx");
		var transform = Markers.parse(content);
		vfs.putContent("Main.hx", transform.source);
		vfs.putContent("Macro.hx", getTemplate("issues/Issue11995/Macro.hx"));
		var args = ["Main"];
		runHaxe(args, () -> {
			assertSuccess();

			function doTest() {
				runHaxeJsonCb(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(1)}, (res) -> {
					Assert.equals(":test", res?.item?.args?.meta?.pop()?.name);
				}, () -> {
					assertSuccess();
					async.done();
				});
			}

			if (invalidate) {
				runHaxeJson(args, ServerMethods.Invalidate, {file: new FsPath("Main.hx")}, () -> {
					doTest();
				});
			} else {
				doTest();
			}
		});
	}
}
