package cases.display.issues;

class Issue5417 extends DisplayTestCase {
	/**
		class Main {
			static var str:String;
			public static function main() {
				Main.{-1-}
			}
		}
	**/
	function test(_) {
		eq(true, hasField(fields(1), "str", "String"));
	}
}
