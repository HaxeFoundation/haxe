package cases.issues;

import haxe.display.Diagnostic;
import haxe.display.FsPath;

class Issue11020 extends TestCase {
	function test(_) {
		vfs.putContent("src/Main.hx", getTemplate("issues/Issue11020/Main.hx"));
		var args = ["-cp", "src", "-main", "Main", "--interp"];
		final res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("src/Main.hx")});
		Assert.equals(1, res.length);
		Assert.equals(4, res[0].diagnostics.length);
		final msgs = [for (d in res[0].diagnostics) (cast d.args : String)];
		Assert.isTrue(msgs.contains("Context.info"));
		Assert.isTrue(msgs.contains("Context.warning"));
		Assert.isTrue(msgs.contains("Context.error"));
		Assert.isTrue(msgs.exists(s -> s.contains("Main.error")));
	}
}
