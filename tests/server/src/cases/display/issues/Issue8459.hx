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
		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(0, sig.activeSignature);
		Assert.equals(1, sig.signatures[0].args.length);
	}
}
