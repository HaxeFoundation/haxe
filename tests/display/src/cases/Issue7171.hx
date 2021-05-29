package cases;

class Issue7171 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				Std.string.bi{-1-}nd(_);
			}
		}
	**/
	function test() {
		eq("(s : Null<Dynamic>) -> String", type(pos(1)));
	}
}
