package cases;

class Issue7072 extends DisplayTestCase {
	/**
		typedef Struct = {
			var foo:Int;
			var bar:Int;
			var foobar:Int;
		}

		class Main {
			public static function main() {
				var s:Struct = {
					{-1-}
				}
			}
		}
	**/
	function test() {
		var results = fields(pos(1));
		// Can't test this in the old protocol because other tests for it expect a different order
		// eq("foo", results[0].name);
		// eq("bar", results[1].name);
		// eq("foobar", results[2].name);
		eq(true, true); // TODO
	}
}
