package cases;

class Issue7940 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				{-1-}"Hello World
			}
		}
	**/
	function test() {
		arrayEq([
			{
				kind: DKParserError,
				range: diagnosticsRange(pos(1), pos(1)),
				severity: Error,
				depth: 0,
				args: "Unterminated string"
			}
		], diagnostics());
	}
}
