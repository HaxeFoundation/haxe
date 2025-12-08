package cases;

class Issue12385 extends DisplayTestCase {
	/**
		typedef Foo = {-1-}String{-2-} & {foo:Int};
		class Main {
			static function main() {
				{-3-}somebody{-4-};
			}
		}
	**/
	function test() {
		final arr:Array<Diagnostic<Dynamic>> = [
			{
				kind: DKUnresolvedIdentifier,
				severity: Error,
				code: null,
				range: diagnosticsRange(pos(3), pos(4)),
				relatedInformation: [],
				args: []
			},
			{
				kind: DKCompilerError,
				severity: Error,
				code: null,
				range: diagnosticsRange(pos(1), pos(2)),
				relatedInformation: [],
				args: "Can only extend structures"
			}
		];
		arrayEq(arr, diagnostics());
	}
}
