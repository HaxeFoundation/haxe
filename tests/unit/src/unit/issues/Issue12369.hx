package unit.issues;

@:unreflective
private class MyIterator {
	var count = 0;
	final max:Int;

	public function new(max:Int) {
		this.max = max;
	}

	public function hasNext():Bool {
		return count < max;
	}

	public function next() {
		return count++;
	}
}

class Issue12369 extends Test {
	#if cpp
	function test() {
		final expected = [0, 1];
		final actual = [];

		for (i in new MyIterator(2)) {
			actual.push(i);
		}

		aeq(expected, actual);
	}
	#end
}
