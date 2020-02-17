package cases;

class Issue7136 extends DisplayTestCase {
	/**
		@:structInit class Point<T> {
			var x:Int;
			var y:T;
		}

		class Main {
			static function main() {
				var p:Point<String> = {
					{-1-}
				}
			}
		}
	**/
	function test() {
		var fields = fields(pos(1));
		eq(2, fields.length);
		eq("x", fields[0].name);
		eq("y", fields[1].name);
		eq("String", fields[1].type);
	}
}
