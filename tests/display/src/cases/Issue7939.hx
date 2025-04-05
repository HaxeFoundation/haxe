package cases;

class Issue7939 extends DisplayTestCase {
	/**
		typedef Struct {-1-}{{-2-}}
	**/
	function test() {
		arrayEq([
			{
				kind: DKParserError,
				range: diagnosticsRange(pos(1), pos(2)),
				severity: Error,
				code: null,
				relatedInformation: [],
				args: "Unexpected {"
			}
		], diagnostics());
	}
}
