package cases.display.issues;

class Issue6997 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				"".lastIndexOf.bind({-1-}
			}
		}
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(0, sig.result.activeSignature);
		Assert.equals(0, sig.result.activeParameter);
		Assert.equals(2, sig.result.signatures[0].args.length);
	}

	/**
		class Main {
			static function main() {
				"".lastIndexOf.bind("foo", {-1-}
			}
		}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(0, sig.result.activeSignature);
		Assert.equals(1, sig.result.activeParameter);
		Assert.equals(2, sig.result.signatures[0].args.length);
	}
}
