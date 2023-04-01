package cases;

class Issue7945 extends DisplayTestCase {
	/**
		abstract Test(Int) {-1-}too{-2-} Int {}
	**/
	function test1() {
		arrayEq([
			{
				kind: DKParserError,
				severity: Error,
				depth: 0,
				range: diagnosticsRange(pos(1), pos(2)),
				args: "Expected { or to or from"
			}
		], diagnostics());
	}

	/**
		class Test {-1-}extend{-2-} OtherClass {}
	**/
	function test2() {
		arrayEq([
			{
				kind: DKParserError,
				severity: Error,
				depth: 0,
				range: diagnosticsRange(pos(1), pos(2)),
				args: "Expected extends or implements or {"
			}
		], diagnostics());
	}
}
