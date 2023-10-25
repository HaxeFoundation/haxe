package cases;

class Issue7777 extends DisplayTestCase {
	/**
		{-1-}import misc.issue7777.Thing;{-2-}
		import misc.issue7777.Foo;

		class Main {
			public static function main() {
				var foo:Foo<String> = BOO;
				trace(foo);
			}
		}

	**/
	function test() {
		arrayEq([
			{
				kind: DKUnusedImport,
				severity: Warning,
				code: null,
				range: diagnosticsRange(pos(1), pos(2)),
				relatedInformation: [],
				args: []
			}
		], diagnostics());
	}
}
