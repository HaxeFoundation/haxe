package cases;

class Issue5306 extends DisplayTestCase {
	/**
		{-1-}import {-3-}InvalidType{-4-};{-2-}

		class Main {
			static function main() {
				var ib:Array<Int>;
				{-5-}ib{-6-}[0] = 0; ib[1] = 1; ib[2]
				{-7-}trace{-8-}("test");
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
				kind: DKParserError,
				range: diagnosticsRange(pos(7), pos(8)),
				severity: Error,
				code: null,
				relatedInformation: [],
				args: "Missing ;"
			},
			{
				kind: DKCompilerError,
				range: diagnosticsRange(pos(3), pos(4)),
				severity: Error,
				code: null,
				relatedInformation: [],
				args: "Type not found : InvalidType"
			},
			{
				kind: DKCompilerError,
				range: diagnosticsRange(pos(5), pos(6)),
				severity: Error,
				code: null,
				relatedInformation: [],
				args: "Local variable ib used without being initialized"
			}
		];
		arrayEq(expected, diagnostics());
	}
}
