package cases;

class Issue6020 extends DisplayTestCase {
	/**
		class C {
			function g(s) {
				(s : String);
				s.{-1-}
			}
		}
	**/
	function test1() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}

	/**
		class C {
			function g(s) {
				s = "foo";
				s.{-1-}
			}
		}
	**/
	function test2() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}
