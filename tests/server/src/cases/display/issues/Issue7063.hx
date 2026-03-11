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
		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(0, sig.activeSignature);
		Assert.equals(2, sig.signatures[0].args.length);
	}
}
