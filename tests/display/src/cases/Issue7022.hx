package cases;

class Issue7022 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {}

			public function ne{-1-}w() {
				new Main();
			}
		}
	**/
	function test() {
		eq("() -> cases.Main", type(pos(1)));
	}
}
