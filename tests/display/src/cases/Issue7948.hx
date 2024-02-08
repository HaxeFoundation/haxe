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
				code: null,
				range: diagnosticsRange(pos(1), pos(2)),
				relatedInformation: [],
				args: 'Unexpected keyword "class"'
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
				code: null,
				range: diagnosticsRange(pos(1), pos(2)),
				relatedInformation: [],
				args: "Unexpected }"
			}
		], diagnostics());
	}
}
