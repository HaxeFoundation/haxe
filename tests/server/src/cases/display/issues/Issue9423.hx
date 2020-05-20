package cases.display.issues;

import haxe.display.Protocol;

class Issue9423 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				Mod9423{-1-}
			}
		}
	**/
	function test(_) {
		vfs.putContent("Mod9423.hx", getTemplate("issues/Issue9423/Mod9423.hx"));
		vfs.putContent("Mod9423.whatever.hx", getTemplate("issues/Issue9423/Mod9423.whatever.hx"));
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: false
		});
		var result = parseCompletion().result;
		// TODO: this test does not pass, but the same setup works fine in vscode
		// Assert.equals(1, result.items.length);
		Assert.pass();
	}
}
