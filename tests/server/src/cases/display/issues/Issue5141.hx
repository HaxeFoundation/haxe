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
		Assert.equals("MyHandler", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.args.path.typeName);

		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(2, sig.signatures[0].args.length);
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
		Assert.equals("MyCallable", runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.args.path.typeName);

		var sig = runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(2), wasAutoTriggered: false});
		Assert.isTrue(sig != null && sig.signatures.length > 0);
		Assert.equals(2, sig.signatures[0].args.length);
	}
}
