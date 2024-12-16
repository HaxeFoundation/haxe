// Array

var a = [1, 2];
var b = haxe.runtime.Copy.copy(a);
1 == b[0];
2 == b[1];
a != b;
var c = [a, a];
var d = haxe.runtime.Copy.copy(c);
d[0] != a;
d[1] != a;
d[0] == d[1];
// List
var l = new haxe.ds.List();
l.add(1);
l.add(2);
var lCopy = haxe.runtime.Copy.copy(l);
1 == lCopy.pop();
2 == lCopy.pop();
l != lCopy;
var l = new haxe.ds.List<Dynamic>();
l.add(l);
var lCopy = haxe.runtime.Copy.copy(l);
l != lCopy;
lCopy == lCopy.pop();
// Anon

var a = {f1: 1, f2: 2};
var b = haxe.runtime.Copy.copy(a);
1 == b.f1;
2 == b.f2;
a != b;
var c = {f1: a, f2: a};
var d = haxe.runtime.Copy.copy(c);
d.f1 != a;
d.f2 != a;
d.f1 == d.f2;
// Enum

var a = (macro 1);
var b = haxe.runtime.Copy.copy(a);
a != b;
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
c != d;
c.ref = c;
var d = haxe.runtime.Copy.copy(c);
c != d;
d == d.ref;
// StringMap
var map = new haxe.ds.StringMap<Dynamic>();
map.set("foo", map);
var mapCopy = haxe.runtime.Copy.copy(map);
map != mapCopy;
mapCopy == mapCopy.get("foo");
// IntMap
var map = new haxe.ds.IntMap<Dynamic>();
map.set(0, map);
var mapCopy = haxe.runtime.Copy.copy(map);
map != mapCopy;
mapCopy == mapCopy.get(0);
// ObjectMap
var map = new haxe.ds.ObjectMap<{}, Dynamic>();
var key = {};
map.set(key, map);
var mapCopy = haxe.runtime.Copy.copy(map);
map != mapCopy;
var keyCopy = [for (key in mapCopy.keys()) key][0];
t(mapCopy == mapCopy.get(keyCopy));
key != keyCopy;
// Bytes
var bytes = haxe.io.Bytes.ofString("foo");
var bytesCopy = haxe.runtime.Copy.copy(bytes);
bytes != bytesCopy;
bytesCopy.getString(0, 3) == "foo";
