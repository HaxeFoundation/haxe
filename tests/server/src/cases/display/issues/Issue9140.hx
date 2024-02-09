package cases.display.issues;

class Issue9140 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var c = new C();
				var f {-1-}= c.field{-2-};
		{-3-}		c.field{-4-} = f;
			}
		}

		class C implements I {
			public var field(g{-5-}et,s{-6-}et):Int;
			public function new() {}

			function get{-7-}_field() return 10;
			function set_f{-8-}ield(value:Int) return value;
		}

		interface I {
			var field(get,set):Int;
		}
	**/
	function test(_) {
		// TODO: The starting positions are off by 4. This comes from patch_string_pos in
		// statistics.ml subtracting the length of get_/set_, while the syntax has the plain
		// field. This is not a new issue though.
		runHaxeJson([], DisplayMethods.FindReferences, {
			file: file,
			kind: WithBaseAndDescendants,
			offset: offset(5)
		});
		var result = parseGotoDefinitionLocations();
		Assert.same([range(1, 2)], result.map(l -> l.range));

		runHaxeJson([], DisplayMethods.FindReferences, {
			file: file,
			kind: WithBaseAndDescendants,
			offset: offset(7)
		});
		var result = parseGotoDefinitionLocations();
		Assert.same([range(1, 2)], result.map(l -> l.range));

		runHaxeJson([], DisplayMethods.FindReferences, {
			file: file,
			kind: WithBaseAndDescendants,
			offset: offset(6)
		});
		var result = parseGotoDefinitionLocations();
		Assert.same([range(3, 4)], result.map(l -> l.range));

		runHaxeJson([], DisplayMethods.FindReferences, {
			file: file,
			kind: WithBaseAndDescendants,
			offset: offset(8)
		});
		var result = parseGotoDefinitionLocations();
		Assert.same([range(3, 4)], result.map(l -> l.range));
	}
}
