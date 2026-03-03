package cases.display.issues;

class Issue5967 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				function call():Void { };
				call({-1-}a{-2-},{-3-},{-4-} {-5-}
			}
		}
	**/
	function test1(_) {
		for (i in [1, 2]) {
			runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(i), wasAutoTriggered: false});
			var sig = parseSignatureHelp();
			Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
			Assert.equals(0, sig.result.activeParameter);
			Assert.equals(0, sig.result.signatures[0].args.length);
		}
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(3), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(1, sig.result.activeParameter);

		for (i in [4, 5]) {
			runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(i), wasAutoTriggered: false});
			sig = parseSignatureHelp();
			Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
			Assert.equals(2, sig.result.activeParameter);
		}
	}
}
