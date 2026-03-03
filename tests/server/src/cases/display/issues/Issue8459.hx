package cases.display.issues;

class Issue8459 extends DisplayTestCase {
	/**
		class Some {
			function main() {
				$type({-1-}
			}
		}
	**/
	function testDollarType(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(0, sig.result.activeSignature);
		Assert.equals(1, sig.result.signatures[0].args.length);
	}
}
