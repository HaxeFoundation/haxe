package cases.display.issues;

class Issue7102 extends DisplayTestCase {
	/**
		import haxe.Constraints.Constructible;
		class Main {
			@:generic static function main<T, TConstructible:Constructible<()->Void>>() {
				new TConstructible({-1-});
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(0, sig.result.signatures[0].args.length);
	}
}
