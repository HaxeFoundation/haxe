package cases;

class Issue7932 extends DisplayTestCase {
	/**
		class Main< {-1-}{{-2-}
			public static function main() {}
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
				args: "Expected type parameter"
			}
		], diagnostics());
	}
}
