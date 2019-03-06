package cases;

class Issue7699 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				#if !macro
				foo("".{-1-});
				#end
			}

			static macro function foo(s:String) {
				return macro {};
			}
		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}
