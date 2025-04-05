package cases;

class Issue7947 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				var either = haxe.ds.Either.Left(123);
				return switch either {
					case Left(_):
						trace('Some logic...');
						trace('Some logic...');
						trace('Some logic...');
						true;
					{-1-}case Right(_):{-2-}
				}
			}
		}
	**/
	function test1() {
		arrayEq([
			{
				kind: DKCompilerError,
				severity: Error,
				code: null,
				range: diagnosticsRange(pos(1), pos(2)),
				relatedInformation: [],
				args: "Void should be Bool"
			}
		], diagnostics());
	}

	/**
		class Main {
			public static function main() {
				var either = haxe.ds.Either.Left(123);
				return switch either {
					case Right(_):
					case Left(_):
						trace('Some logic...');
						trace('Some logic...');
						trace('Some logic...');
						{-1-}true{-2-};
				}
			}
		}
	**/
	function test2() {
		arrayEq([
			{
				kind: DKCompilerError,
				severity: Error,
				code: null,
				range: diagnosticsRange(pos(1), pos(2)),
				relatedInformation: [],
				args: "Bool should be Void"
			}
		], diagnostics());
	}
}
