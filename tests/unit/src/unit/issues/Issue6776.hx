package unit.issues;

class Issue6776 extends unit.Test {
	function test1() {
        var a = [1];
        var i = 0;
        var v = a[i];
        a[i]++;
        eq(1, v);
	}

	function test2() {
        var a = [1];
        var i = 0;
        var v = a[i];
        a[i] += 1;
        eq(1, v);
	}
}