package cases.display.issues;

class Issue8061 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				new sys.io.Process({-1-})
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseSignatureHelp();
		Assert.isTrue(result.result.signatures[0].documentation != null);
	}
}