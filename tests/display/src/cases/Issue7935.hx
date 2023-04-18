package cases;

class Issue7935 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				trace({-1-};{-2-}
			}
		}
	**/
	function test1() {
		hasParserError("Expected expression or )");
	}

	/**
		class Main {
			public static function main() {
				trace(0
			{-1-}}{-2-}
		}
	**/
	function test2() {
		hasParserError("Expected , or )");
	}

	/**
		class Main {
			public static function main() {
				trace(0,
			{-1-}}{-2-}
		}
	**/
	function test3() {
		hasParserError("Expected expression");
	}

	function hasParserError(message:String) {
		arrayEq([
			{
				kind: DKParserError,
				range: diagnosticsRange(pos(1), pos(2)),
				severity: Error,
				relatedInformation: [],
				args: message
			}
		], diagnostics());
	}
}
