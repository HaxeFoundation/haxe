package unit.issues;

class Issue3845 extends Test {
	function test() {
		var o = {};

		var m = new haxe.ds.ObjectMap();
		m.set(o, 1);
		m.set(o, 2);

		eq(2, m.get(o));
	}
}