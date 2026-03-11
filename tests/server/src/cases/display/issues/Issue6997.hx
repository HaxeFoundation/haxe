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
		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(0, sig.activeSignature);
		Assert.equals(0, sig.activeParameter);
		Assert.equals(2, sig.signatures[0].args.length);
	}

	/**
		class Main {
			static function main() {
				"".lastIndexOf.bind("foo", {-1-}
			}
		}
	**/
	function test2(_) {
		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(0, sig.activeSignature);
		Assert.equals(1, sig.activeParameter);
		Assert.equals(2, sig.signatures[0].args.length);
	}
}
