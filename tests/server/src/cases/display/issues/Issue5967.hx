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
			var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(i), wasAutoTriggered: false});
			Assert.isTrue(sig != null && sig.signatures.length > 0);
			Assert.equals(0, sig.activeParameter);
			Assert.equals(0, sig.signatures[0].args.length);
		}
		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(3), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(1, sig.activeParameter);

		for (i in [4, 5]) {
			sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(i), wasAutoTriggered: false});
			Assert.isTrue(sig != null && sig.signatures.length > 0);
			Assert.equals(2, sig.activeParameter);
		}
	}
}
