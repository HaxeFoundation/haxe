package cases._issue5417;

class Issue5417 extends DisplayTestCase {
	/**
	class Main {
		static var str:String;
		public static function main() {
			Main.{-1-}
		}
	}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "str", "String"));
	}
}