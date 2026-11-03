package cases.display.issues;

class Issue7703 extends DisplayTestCase {
	/**
		class Main {
		public static function main() {
			f{-1-}oo({-2-}"");
			{-3-}
		}

		static macro function foo(s:String) {
			return macro {};
		}
		}
	**/
	function test(_) {
		Assert.isTrue(runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.kind == (cast "TFun" : Dynamic));

		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(1, sig.signatures[0].args.length);
		Assert.equals("String", sig.signatures[0].args[0].t.args.path.typeName);
	}
}
