package unit.issues;

class Issue6859 extends unit.Test {
	function test() {
		var l = new List();
        l.add(42);
        l.add(42);
        l.pop();
        l.add(42);
        l.pop();
        l.pop();
        eq(0, l.length);
	}
}