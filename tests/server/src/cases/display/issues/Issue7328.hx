package cases.display.issues;

class Issue7328 extends DisplayTestCase {
	/**
		import haxe.macro.Expr.ExprDef;

		class Main {
			public static function main() {
				switch (null:ExprDef) {
					case EConst(CIdent({-1-}s)):
					case _:
				}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(0, sig.result.activeSignature);
		Assert.equals(1, sig.result.signatures[0].args.length);
		Assert.equals("String", sig.result.signatures[0].args[0].t.args.path.typeName);
	}
}
