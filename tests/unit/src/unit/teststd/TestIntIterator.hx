package unit.teststd;

class TestIntIterator extends unit.Test {
	public function test() {
		var ii = new IntIterator(0, 2);
		t(ii.hasNext());
		eq(ii.next(), 0);
		t(ii.hasNext());
		eq(ii.next(), 1);
		f(ii.hasNext());
		var ii = new IntIterator(0, 2);
		var r = [];
		for (i in ii)
			r.push(i);
		aeq([0, 1], r);
		for (i in ii)
			r.push(i);
		aeq([0, 1], r);
	}
}
