package unit.issues;

class Issue5027 extends Test {
	function test() {
		var f:()->Void = function() return null;
		f();
		noAssert();
	}
}