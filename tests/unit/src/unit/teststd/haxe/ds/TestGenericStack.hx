package unit.teststd.haxe.ds;

class TestGenericStack extends unit.Test {
	public function test() {
		var gs = new haxe.ds.GenericStack<String>();
		t(gs.isEmpty());
		eq(gs.first(), null);
		eq(gs.pop(), null);
		f(gs.remove(null));
		gs.add("foo");
		f(gs.isEmpty());
		eq(gs.first(), "foo");
		eq(gs.pop(), "foo");
		t(gs.isEmpty());
		eq(gs.first(), null);
		eq(gs.pop(), null);
		gs.add("foo");
		eq(gs.first(), "foo");
		t(gs.remove("foo"));
		t(gs.isEmpty());
		gs.add("foo");
		gs.add("bar");
		eq(gs.pop(), "bar");
		eq(gs.first(), "foo");
		eq(gs.pop(), "foo");
		gs.add(null);
		gs.add(null);
		f(gs.isEmpty());
		eq(gs.first(), null);
		eq(gs.pop(), null);
		t(gs.remove(null));
		t(gs.isEmpty());
		eq(gs.first(), null);
		eq(gs.pop(), null);
	}
}
