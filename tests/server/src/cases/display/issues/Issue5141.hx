package cases.display.issues;

class Issue5141 extends DisplayTestCase {
	/**
		typedef MyHandler = Int->String->Void

		class Some {
			function main() {
				var a:MyHandler;
				a{-1-};
				a({-2-}
			}
		}
	**/
	function testTypedef(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("MyHandler", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(2, sig.result.signatures[0].args.length);
	}

	/**
		@:callable
		abstract MyCallable(Int->String->Void) {}

		class Some {
			function main() {
				var a:MyCallable;
				a{-1-};
				a({-2-}
			}
		}
	**/
	function testAbstract(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("MyCallable", parseHover().result.item.type.args.path.typeName);

		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		var sig = parseSignatureHelp();
		Assert.isTrue(sig.result != null && sig.result.signatures.length > 0);
		Assert.equals(2, sig.result.signatures[0].args.length);
	}
}
