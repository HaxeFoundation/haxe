package cases;

class Issue7877 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				new misc.issue7877.ProcessedClass(false);
				new misc.issue7877.ProcessedClass(true);
			}
		}
	**/
	function test() {
		arrayEq([], diagnostics());
	}
}
