package cases.display.issues;

class Issue9087 extends DisplayTestCase {
	/**
		class A {
			public function new() {
				in{-1-}it(); // "go to implementation" from the call site currently yields nothing
			}

			function init() {}
		}

		class B extends A {
			override function {-2-}init{-3-}() {
				super.init();
			}
		}
	**/
	function test(async:utest.Async) {
		runHaxeJson([], DisplayMethods.GotoImplementation, {file: file, offset: offset(1), contents: source});
		var result = parseGotoDefintion().result;
		Assert.equals(1, result.length);
		Assert.same(range(2, 3), result[0].range);
	}
}