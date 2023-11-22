package cases.display.issues;

import haxe.display.Diagnostic;

class Issue9705 extends TestCase {
	function test(_) {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9705/Main.hx"));
		var args = ["Main", "--interp"];
		runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));

		var diag = parseDiagnostics();
		var range = diag[0].range;
		Assert.equals(DKUnresolvedIdentifier, diag[0].kind);
		Assert.equals("Arrey".length, range.end.character - range.start.character);
	}
}
