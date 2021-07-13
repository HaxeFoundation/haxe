package unit.issues;

import utest.Assert;

private class PairIter<T> {
	var data:Array<T>;
	var x = 0;
	var prev:T;
	var cur:T;

	public inline function new(data:Array<T>) {
		this.data = data;
	}

	public inline function hasNext() {
		return x < data.length;
	}

	public inline function next() {
		prev = cur;
		cur = data[x++];
		return {prev: prev, cur: cur};
	}
}

class Issue10304 extends Test {
	function test() {
		var buf = new StringBuf();
		for (p in new PairIter(["1", "2", "3"])) {
			buf.add('${p.prev} ${p.cur} ');
		}
		eq("null 1 1 2 2 3 ", buf.toString());
	}
}
