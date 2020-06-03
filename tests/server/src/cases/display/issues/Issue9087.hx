package cases.display.issues;

class Issue9087 extends DisplayTestCase {
	/**
		class A {
			public function new() {
				in{-1-}it(); // "go to implementation" from the call site currently yields nothing
			}

			function init() {}
		}

		class Main extends A {
			override function {-2-}init{-3-}() {
				super.init();
			}

			static public function main() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.GotoImplementation, {file: file, offset: offset(1), contents: source});
		trace(Sys.environment());
		js.Syntax.code('console.log({0})', lastResult.stderr);
		js.Syntax.code('console.log({0})', haxe.Json.parse(lastResult.stderr));
		var result = parseGotoDefintion().result;
		Assert.equals(1, result.length);
		Assert.same(range(2, 3), result[0].range);
	}
}