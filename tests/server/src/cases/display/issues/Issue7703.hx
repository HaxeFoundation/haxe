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
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));

		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(1, sig.result.signatures[0].args.length);
		Assert.equals("String", sig.result.signatures[0].args[0].t.args.path.typeName);
	}
}
