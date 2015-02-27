package unit.issues;

class Issue3414 extends Test {
	function test() {
		var x = 200015;
		eq(-1, ((((x << 28)) >> 28)));
		eq(-1, (((x << 28) & 0xffffffff) >> 28));
	}
}