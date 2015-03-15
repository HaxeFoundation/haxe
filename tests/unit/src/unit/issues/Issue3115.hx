package unit.issues;

class Issue3115 extends Test {
	function test() {
        var k = 0;
        var i = 0;
        do {
            i++;
            if (i & 1 == 0) continue;
            k++;
        } while (i++ < 10);
		eq(12, i);
		eq(6, k);
	}
}