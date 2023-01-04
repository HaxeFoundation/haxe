package unit.issues;

class Issue10752 extends Test {
	function shl(x, y)
		return x << y;

	#if (!php && !python && !lua)
	function test() {
		eq(2, shl(1, 33));
		eq(2, 1 << 33);

		eq(1 >> 1, 1 >> 33);
		eq(1 >>> 1, 1 >>> 33);
	}
	#end
}
