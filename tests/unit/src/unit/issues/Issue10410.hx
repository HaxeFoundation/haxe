package unit.issues;

class Issue10410 extends Test {
#if js
	function test() {
		var n = cast("5", String); // including this to add the code due to DCE, otherwise remove this if no DCE
		var fields = [];
		js.Syntax.code("for (var s in {0}) {1}.push(s)", n, fields);
		f(fields.contains('__class__'));
	}
#end
}
