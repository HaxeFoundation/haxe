package cases.display.issues;

class Issue7252 extends DisplayTestCase {
	/**
		class Main {
			var a : Array<Int>;
			function foo() {
				a.map(function(_) {
					a.concat({-1-})
				});
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(0, sig.result.activeSignature);
		Assert.equals(1, sig.result.signatures[0].args.length);
		Assert.equals("Array", sig.result.signatures[0].args[0].t.args.path.typeName);
	}
}
