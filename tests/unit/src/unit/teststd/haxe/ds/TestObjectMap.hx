package unit.teststd.haxe.ds;

class TestObjectMap extends unit.Test {
	public function test() {
		var k1 = new IntWrap(1);
		var k2 = new IntWrap(2);
		var k3 = new IntWrap(3);
		var o = new haxe.ds.ObjectMap();

		// non existent
		f(o.exists(k1));
		f(o.exists(k2));
		f(o.exists(k3));
		eq(o.get(k1), null);
		eq(o.get(k2), null);
		eq(o.get(k3), null);

		// set + exists
		o.set(k1, "9");
		o.set(k2, "8");
		o.set(k3, "7");
		t(o.exists(k1));
		t(o.exists(k2));
		t(o.exists(k3));

		// the __id__ field should not appear in Reflect.fields
		#if js
		var fields = Reflect.fields(k1);
		eq(fields[0], "i");
		#end

		// get
		eq(o.get(k3), "7");
		eq(o.get(k2), "8");
		eq(o.get(k1), "9");

		// keys
		var a = [];
		for (k in o.keys())
			a.push(k);
		eq(a.length, 3);
		t(a[0] == k1 || a[0] == k2 || a[0] == k3);
		t(a[1] == k1 || a[1] == k2 || a[1] == k3);
		t(a[2] == k1 || a[2] == k2 || a[2] == k3);
		t(o.exists(k1));
		t(o.exists(k2));
		t(o.exists(k3));
		eq(o.get(k3), "7");
		eq(o.get(k2), "8");
		eq(o.get(k1), "9");

		// iterator
		var a:Array<String> = [];
		for (k in o) {
			a.push(k);
		}
		eq(a.length, 3);
		t(a[0] == "9" || a[0] == "8" || a[0] == "7");
		t(a[1] == "9" || a[1] == "8" || a[1] == "7");
		t(a[2] == "9" || a[2] == "8" || a[2] == "7");
		t(o.exists(k1));
		t(o.exists(k2));
		t(o.exists(k3));
		eq(o.get(k3), "7");
		eq(o.get(k2), "8");
		eq(o.get(k1), "9");

		// remove
		t(o.remove(k2));
		t(o.exists(k1));
		f(o.exists(k2));
		t(o.exists(k3));
		eq(o.get(k1), "9");
		eq(o.get(k2), null);
		eq(o.get(k3), "7");
		var a = [];
		for (k in o.keys())
			a.push(k);
		eq(a.length, 2);
		t(a[0] == k1 || a[0] == k3);
		t(a[1] == k1 || a[1] == k3);
		var a:Array<String> = [];
		for (k in o.iterator()) {
			a.push(k);
		}
		eq(a.length, 2);
		t(a[0] == "9" || a[0] == "7");
		t(a[1] == "9" || a[1] == "7");
		f(o.remove(k2));

		// clear
		o.clear();
		eq(o.get(k1), null);
		f(o.exists(k1));
		f(o.exists(k2));
		f(o.exists(k3));
		var a = [for (k in o.keys()) k];
		eq(a.length, 0);

	}
}
