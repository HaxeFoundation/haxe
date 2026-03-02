package cases.display.issues;

class Issue6068 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var a:{i:Int};
				a({-1-});

				Main({-2-});
			}
		}
	**/
	function test(_) {
		// Calling non-callable types should yield empty/error signature help
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result == null || sig.result.signatures.length == 0);

		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		sig = parseSignatureHelp();
		Assert.isTrue(sig.result == null || sig.result.signatures.length == 0);
	}
}
