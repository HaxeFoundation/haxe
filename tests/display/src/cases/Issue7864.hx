package cases;

class Issue7864 extends DisplayTestCase {
	/**
		@{-1-}
		class Main {
			static function main() {
			}
		}

		@{-2-}
		class Test {}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "@:enum", "", "metadata"));
		eq(true, hasField(fields(pos(2)), "@:enum", "", "metadata"));
	}
}
