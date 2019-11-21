package unit.issues;

private abstract Dummy(Iterable<Int>) from Iterable<Int> {
	@:to inline function toIter():Iterable<Int> {
		return this;
	}
}

class Issue8984 extends unit.Test {
	function test() {
		var m = [1 => 2, 3 => 4];
		eq(6, direct(m));
		eq(6, constraint(m));

		var d:Dummy = [2, 4];
		eq(6, direct(d));
		eq(6, constraint(d));
	}

	static function direct(it:Iterable<Int>):Int {
		var sum = 0;
		for(i in it) {
			sum += i;
		}
		return sum;
	}

	static function constraint<T:Iterable<Int>>(it:T):Int {
		var sum = 0;
		for(i in it) {
			sum += i;
		}
		return sum;
	}
}