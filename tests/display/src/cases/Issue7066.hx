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
		eq("fieldB", results[0].name);
		eq("var", results[0].kind);
		eq("Null<String>", results[0].type);
	}
}
