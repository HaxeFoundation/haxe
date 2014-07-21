package unit.issues;

class Issue3197 extends Test {
	function test() {
        var v = "good";
        for (i in 0...10) {
            v = if (i == 1) "bad" else break;
        }
        eq(v, "good");

        var a = [];
        for (i in 0...10) {
            var v = if (i == 1) i else continue;
            a.push(v);
        }
        eq(a.length, 1);
        eq(a[0], 1);
	}
}
