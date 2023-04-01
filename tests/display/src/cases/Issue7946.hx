package cases;

class Issue7946 extends DisplayTestCase {
	/**
		{-1-}open{-2-} haxe.Json;
	**/
	function test1() {
		arrayEq([
			{
				kind: DKParserError,
				severity: Error,
				depth: 0,
				range: diagnosticsRange(pos(1), pos(2)),
				args: "Unexpected open"
			}
		], diagnostics());
	}

	/**
		{-1-}clas{-2-} Test {}
	**/
	function test() {
		arrayEq([
			{
				kind: DKParserError,
				severity: Error,
				depth: 0,
				range: diagnosticsRange(pos(1), pos(2)),
				args: "Unexpected clas"
			}
		], diagnostics());
	}
}
