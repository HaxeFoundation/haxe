package cases.display.issues;

class Issue9383 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				{-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson(["-v"], DisplayMethods.SignatureHelp, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: false,
		});
		Assert.isFalse(Lambda.exists(messages, msg -> msg.contains("Typing macro Main.main")));
		Assert.isFalse(Lambda.exists(messages, msg -> msg.contains("Typing macro")));
	}
}