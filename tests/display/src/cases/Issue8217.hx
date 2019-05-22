package cases;

class Issue8217 extends DisplayTestCase {
	/**
		class Main {
			static function foo( cmd : String ) {
				switch( cmd ) {
				case "user/login":
					var o = {
						x : 55,
						str : "hello".{-1-}
						z : 66,
					};
				}
			}

		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}
