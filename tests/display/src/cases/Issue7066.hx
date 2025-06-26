package cases;

class Issue7066 extends DisplayTestCase {
	/**
		typedef Struct = {
			?fieldA:Int,
			?fieldB:String
		}

		class Main {
			static function main() {}
			function foo():Struct {
				return {
					fieldA: 5,
					{-1-}
				};
			}
		}
	**/
	function test() {
		var results = fields(pos(1));
		eq(1, results.length);
		eq(true, isField(results[0], "fieldB", "Null<String>", "var"));
	}
}
