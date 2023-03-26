package cases;

class Issue7943 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				{-1-}0{-2-}{-3-}1{-4-};
			}
		}
	**/
	function test() {
		arrayEq([
			{
				kind: DKParserError,
				severity: Error,
				range: diagnosticsRange(pos(3), pos(4)),
				args: "Missing ;"
			}
		], diagnostics());
	}
}
