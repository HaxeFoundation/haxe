package cases;

class Issue7938 extends DisplayTestCase {
	/**
		class Main {
			public static {-1-}fuction{-2-} main() {}
		}
	**/
	function test() {
		arrayEq([
			{
				kind: DKParserError,
				range: diagnosticsRange(pos(1), pos(2)),
				severity: Error,
				code: null,
				relatedInformation: [],
				args: "Unexpected fuction"
			}
		], diagnostics());
	}
}
