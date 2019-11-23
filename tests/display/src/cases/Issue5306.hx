package cases;

class Issue5306 extends DisplayTestCase {
	/**
		{-1-}import {-3-}InvalidType{-4-};{-2-}

		class Main {
			static function main() {
				var ib:Array<Int>;
				ib[0] = 0; ib[1] = 1; ib[2]
				{-5-}trace{-6-}("test");
			}
		}
	**/
	function test() {
		var expected:Array<Diagnostic<Dynamic>> = [
			// {
			// 	kind: DKUnusedImport,
			// 	range: diagnosticsRange(pos(1), pos(2)),
			// 	severity: Warning,
			// 	args: []
			// },
			{
				kind: DKCompilerError,
				range: diagnosticsRange(pos(3), pos(4)),
				severity: Error,
				args: "Type not found : InvalidType"
			},
			{
				kind: DKParserError,
				range: diagnosticsRange(pos(5), pos(6)),
				severity: Error,
				args: "Missing ;"
			}
		];
		arrayEq(expected, diagnostics());
	}
}
