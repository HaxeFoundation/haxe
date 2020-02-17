package cases;

class Issue7948 extends DisplayTestCase {
	/**
		class Main {
			{-1-}class{-2-} Moin {

			}
		}
	**/
	function test() {
		arrayEq([
			{
				kind: DKParserError,
				severity: Error,
				range: diagnosticsRange(pos(1), pos(2)),
				args: "Unexpected class"
			}
		], diagnostics());
	}

	/**
		class Main {
			static function main()
				trace("Test");
			}
		{-1-}}{-2-}
	**/
	function test2() {
		arrayEq([
			{
				kind: DKParserError,
				severity: Error,
				range: diagnosticsRange(pos(1), pos(2)),
				args: "Unexpected }"
			}
		], diagnostics());
	}
}
