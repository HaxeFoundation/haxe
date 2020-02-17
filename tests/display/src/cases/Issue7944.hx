package cases;

class Issue7944 extends DisplayTestCase {
	/**
		class Main {
		static function main() {}

		{-1-}fun{-2-} f() {}
		}
	**/
	function test() {
		arrayEq([
			{
				kind: DKParserError,
				severity: Error,
				range: diagnosticsRange(pos(1), pos(2)),
				args: "Unexpected fun"
			}
		], diagnostics());
	}
}
