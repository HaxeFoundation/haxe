package unit.teststd;

class TestList extends unit.Test {
	public function test() {
		var l = new List();
		eq(l.toString(), "{}");
		t(l.isEmpty());
		f(l.remove("1"));
		eq(l.length, 0);
		eq(l.first(), null);
		eq(l.last(), null);
		eq(l.pop(), null);
		l.add("1");
		eq(l.length, 1);
		eq(l.first(), "1");
		eq(l.last(), "1");
		eq(l.toString(), "{1}");
		f(l.isEmpty());
		eq(l.join("x"), "1");
		eq(l.pop(), "1");
		f(l.remove("1"));
		eq(l.length, 0);
		l.add("1");
		eq(l.length, 1);
		t(l.remove("1"));
		l.add("1");
		l.push("2");
		eq(l.length, 2);
		eq(l.first(), "2");
		eq(l.last(), "1");
		eq(l.toString(), "{2, 1}");
		eq(l.join("x"), "2x1");
		l.clear();
		t(l.isEmpty());
		l.add("1");
		l.add("2");
		l.add("3");
		var l2 = l.map(function(i:String) return i + i);
		eq(l2.pop(), "11");
		eq(l2.pop(), "22");
		eq(l2.pop(), "33");
		var l3 = l.filter(function(i:String) return i != "2");
		eq(l3.pop(), "1");
		eq(l3.pop(), "3");

		// keyValueIterator
		var l4 = new List();
		l4.add(1);
		l4.add(2);
		l4.add(3);
		l4.add(5);
		l4.add(8);
		aeq([0, 1, 2, 3, 4], [for (k=>v in l4) k]);
		aeq([1, 2, 3, 5, 8], [for (k=>v in l4) v]);
		aeq([0, 2, 6, 15, 32], [for (k=>v in l4) k*v]);
	}
}
