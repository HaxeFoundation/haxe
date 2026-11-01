package unit.issues;

class Issue7121 extends Test {
	var push:Array<Int>->Void;

	function _push(array:Array<Int>) {
		eq(array.length, 2);
		eq(array[0], 0);
		eq(array[1], 1);
	}

	static inline function shrink(buf:Array<Int>, size:Int) {
		if (buf.length != size) {
			buf = buf.slice(0, size);
		}
		return buf;
	}

	function test() {
		var array = [0, 1, 2, 3];
		push = _push;
		push(shrink(array, 2));
	}
}
