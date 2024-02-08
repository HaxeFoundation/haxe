package unit.issues;

class Issue9024 extends unit.Test {
#if js
	static var a:String = "a";
	static var b:String = "b";

	function test() {
		eq(2, js.Syntax.code('{0}.length', a + b));
		eq(2, js.Syntax.code('{0}.length', a < b ? "cd" : "efg"));
	}
#end
}