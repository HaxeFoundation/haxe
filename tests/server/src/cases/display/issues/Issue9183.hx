package cases.display.issues;

class Issue9183 extends DisplayTestCase {
	/**
		class A {
			function f(x:Int) {}
			function g(y:Int) {}
		}

		class B extends A {
			override function f(x:Int) {
				g(10); {-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertSuccess();
	}
}
