package cases.issues;

import haxe.display.Diagnostic;
import haxe.display.FsPath;

class Issue6794 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue6794/Main.hx"));
		var args = ["-main", "Main", "--interp"];
		final res = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")});
		Assert.equals(1, res.length);
		Assert.equals(1, res[0].diagnostics.length);
		var d = res[0].diagnostics[0];
		Assert.equals((DKCompilerError : Int), (d.kind : Int));
		Assert.equals(Warning, d.severity);
		Assert.equals("foo", cast d.args);
	}
}
