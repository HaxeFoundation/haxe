package cases.display.issues;

class Issue10008 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				foo(1, {-1-});
			}

			static function foo(...rest:Int) {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseSignatureHelp();
		Assert.equals(0, result.result.activeParameter);
	}
}