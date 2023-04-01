package cases;

class Issue10194 extends DisplayTestCase {
	/**
		function f(a:Int, b:Int) {}

		function main() {
			var v = 10; // reported as "unused variable"
			f({-1-}""{-2-}, v);
		}
	**/
	function test() {
		arrayEq([
			{
				kind: DKCompilerError,
				severity: Error,
				depth: 0,
				range: diagnosticsRange(pos(1), pos(2)),
				args: "String should be Int\nFor function argument 'a'"
			}
		], diagnostics());
	}
}
