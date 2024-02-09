package unit.issues;

class Issue10783 extends Test {
	function test() {
		eq(4, log2Unsigned(16));
	}

	@:pure inline function log2Unsigned(n:Int):Int {
		var res = 0;

		while ((n >>>= 1) != 0) {
			res++;
		}

		return res;
	}
}
