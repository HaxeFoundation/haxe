package cases;

class Issue9843 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				test(() -> {});
			}

			static function test(fn:()->Void) {}
		}
	**/
	function test() {
		arrayEq([], diagnostics());
	}
}
