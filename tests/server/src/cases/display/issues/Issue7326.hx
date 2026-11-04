package cases.display.issues;

class Issue7326 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			public static function main() {
				var o:Option<Int> = None;
				switch (o) {
					case Some({-1-}v):
					case None:
				}
				Some({-2-});
			}
		}
	**/
	function test(_) {
		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(0, sig.activeSignature);
		Assert.equals(1, sig.signatures[0].args.length);
		Assert.equals("Int", sig.signatures[0].args[0].t.args.path.typeName);

		sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(0, sig.activeSignature);
		Assert.equals(1, sig.signatures[0].args.length);
	}
}
