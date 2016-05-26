package unit.issues;

class Issue5029 extends Test {
	function test() {
		var template = "";
		var f = function() template = "some";
		f();
		eq("some", template);
	}
}