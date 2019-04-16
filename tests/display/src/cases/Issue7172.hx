package cases;

class Issue7172 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}

			function foo() {
				this.{-1-}


				var x:Int;
			}
		}
	**/
	function testNo() {
		eq(false, hasField(fields(pos(1)), "x", "Int"));
	}

	/**
		class Main {
			static function main() {}

			function foo() {
				this.{-1-}

			}

				var x:Int;
		}
	**/
	function testYes() {
		eq(true, hasField(fields(pos(1)), "x", "Int"));
	}
}
