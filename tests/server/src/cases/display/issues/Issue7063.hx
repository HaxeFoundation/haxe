package cases.display.issues;

class Issue7063 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				call({
					foo: 1,{-1-}
				});
			}

			static function call(arg1:Dynamic, arg2:Int) { }
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(0, sig.result.activeSignature);
		Assert.equals(2, sig.result.signatures[0].args.length);
	}
}
