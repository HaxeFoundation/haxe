package cases.display.issues;

import haxe.display.Diagnostic;

class Issue9705 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9705/Main.hx"));
		var args = ["Main", "--interp"];
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
			Assert.equals(1, res.length);
			Assert.equals(1, res[0].diagnostics.length);
			Assert.equals(res[0].diagnostics[0].kind, DKUnresolvedIdentifier);

			var range = res[0].diagnostics[0].range;
			Assert.equals("Arrey".length, range.end.character - range.start.character);
		});
	}
}
