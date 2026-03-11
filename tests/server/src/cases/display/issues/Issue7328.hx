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
		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(0, sig.activeSignature);
		Assert.equals(1, sig.signatures[0].args.length);
		Assert.equals("String", sig.signatures[0].args[0].t.args.path.typeName);
	}
}
