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
		runHaxeJson([], DisplayMethods.FindReferences, {file: file, kind: WithBaseAndDescendants, offset: offset(1)});
		var result = parseGotoDefinitionLocations();
		Assert.same([range(2, 3)], result.map(l -> l.range));
		Assert.pass();
	}
}
