package unit.issues;

class Issue5425 extends unit.Test {
	function test() {
        var self = 1;
        function f() self = 2;
        f();
        eq(2, self);
	}
}
