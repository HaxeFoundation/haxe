package cases.display.issues;

class Issue10171 extends TestCase {
	function test(_) {
		var tpl = storeAndParseTemplate("issues/Issue10171/Main.hx", "src/Main.hx");
		var args = ["-cp", "src", "-main", "Main", "--interp"];
		var result = runHaxeJson(args, DisplayMethods.SignatureHelp, {
			file: new haxe.display.FsPath("src/Main.hx"),
			offset: tpl.markers[1],
			wasAutoTriggered: false
		});
		Assert.equals(1, result.signatures.length);
		Assert.equals(0, result.activeSignature);
		Assert.equals(0, result.activeParameter);
		Assert.isTrue(result.signatures[0].documentation.contains("random number generator"));
	}
}
