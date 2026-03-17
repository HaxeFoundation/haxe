package unit.teststd.haxe.runtime;

class TestCopy extends unit.Test {
	public function test() {
		// Array

		var a = [1, 2];
		var b = haxe.runtime.Copy.copy(a);
		eq(1, b[0]);
		eq(2, b[1]);
		t(a != b);
		var c = [a, a];
		var d = haxe.runtime.Copy.copy(c);
		t(d[0] != a);
		t(d[1] != a);
		eq(d[0], d[1]);
		// List
		var l = new haxe.ds.List();
		l.add(1);
		l.add(2);
		var lCopy = haxe.runtime.Copy.copy(l);
		eq(1, lCopy.pop());
		eq(2, lCopy.pop());
		t(l != lCopy);
		var l = new haxe.ds.List<Dynamic>();
		l.add(l);
		var lCopy = haxe.runtime.Copy.copy(l);
		t(l != lCopy);
		eq(lCopy, lCopy.pop());
		// Anon

		var a = {f1: 1, f2: 2};
		var b = haxe.runtime.Copy.copy(a);
		eq(1, b.f1);
		eq(2, b.f2);
		t(a != b);
		var c = {f1: a, f2: a};
		var d = haxe.runtime.Copy.copy(c);
		t(d.f1 != a);
		t(d.f2 != a);
		eq(d.f1, d.f2);
		// Enum

		var a = (macro 1);
		var b = haxe.runtime.Copy.copy(a);
		t(a != b);
		// a.expr != b.expr; // this fails on cpp, but enum instance equality isn't very specified anyway
		switch [a.expr, b.expr] {
			case [EConst(CInt(a)), EConst(CInt(b))]:
				eq(a, b);
			case _:
				utest.Assert.fail('match failure: ${a.expr} ${b.expr}');
		}
		// Class
		var c = new MyClass(0);
		var d = haxe.runtime.Copy.copy(c);
		t(c != d);
		c.ref = c;
		var d = haxe.runtime.Copy.copy(c);
		t(c != d);
		eq(d, d.ref);
		// StringMap
		var map = new haxe.ds.StringMap<Dynamic>();
		map.set("foo", map);
		var mapCopy = haxe.runtime.Copy.copy(map);
		t(map != mapCopy);
		eq(mapCopy, mapCopy.get("foo"));
		// IntMap
		var map = new haxe.ds.IntMap<Dynamic>();
		map.set(0, map);
		var mapCopy = haxe.runtime.Copy.copy(map);
		t(map != mapCopy);
		eq(mapCopy, mapCopy.get(0));
		// ObjectMap
		var map = new haxe.ds.ObjectMap<{}, Dynamic>();
		var key = {};
		map.set(key, map);
		var mapCopy = haxe.runtime.Copy.copy(map);
		t(map != mapCopy);
		var keyCopy = [for (key in mapCopy.keys()) key][0];
		t(mapCopy == mapCopy.get(keyCopy));
		t(key != keyCopy);
		// Bytes
		var bytes = haxe.io.Bytes.ofString("foo");
		var bytesCopy = haxe.runtime.Copy.copy(bytes);
		t(bytes != bytesCopy);
		eq(bytesCopy.getString(0, 3), "foo");

	}
}
