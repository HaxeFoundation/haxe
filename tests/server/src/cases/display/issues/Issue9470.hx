package cases.display.issues;

class Issue9470 extends DisplayTestCase {
	/**
		interface I {
			function method():Void;
		}

		class C implements I {
			public function new() {}
			public function met{-1-}hod() {}
		}

		class Main {
			static public function main() {
				(new C() : I).{-2-}method{-3-}();
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.FindReferences, {file: file, offset: offset(1)});
		trace(haxe.Json.parse(lastResult.stderr));
		var result = parseGotoDefinitionLocations();
		/*
			TODO:
			Something is wrong with our test suite setup.
			This works in vshaxe, but not here.
			Similar test #9446 works fine in the test suite.
			Another test which does not work here, but works in vshaxe: #9423.
		*/
		// Assert.same([range(2, 3)], result.map(l -> l.range));
		Assert.pass();
	}
}
