package cases;

class Issue7935 extends DisplayTestCase {
	/**
	class Main {
		public static function main() {
			trace({-1-};{-2-}
		}
	}
	**/
	function test() {
		arrayEq([{
			kind: DKParserError,
			range: diagnosticsRange(pos(1), pos(2)),
			severity: Error,
			args: "Expected expression or )"
		}], diagnostics());
	}
}
