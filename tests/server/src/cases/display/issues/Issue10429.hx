package cases.display.issues;

class Issue10429 extends DisplayTestCase {
	/**
		import haxe.macro.Expr;

		typedef ArgType = {
			var f:String;
		}

		class Main {
			public static function main() {
				f{-1-}oo({-2-});
				{-3-}
			}

			static macro function foo(s:String, e:ExprOf<String>, o:ArgType) {
				return macro {};
			}
		}
	**/
	function test(_) {
		Assert.isTrue(runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.kind == (cast "TFun" : Dynamic));

		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(0, sig.activeSignature);
		Assert.equals(3, sig.signatures[0].args.length);
	}
}
