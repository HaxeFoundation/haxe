package unit.std;

class TestArray extends unit.Test {
	public function test() {
		// length
		eq([].length, 0);
		eq([1].length, 1);
		var a = [];
		a[4] = 1;
		eq(a.length, 5);
		a[a.length] = 1;
		eq(a.length, 6);

		// concat
		eq([].concat([]).length, 0);
		eq([1].concat([])[0], 1);
		eq([].concat([1])[0], 1);
		eq([1].concat([2])[0], 1);
		eq([1].concat([2])[1], 2);
		eq([1,2].concat([2,1])[0], 1);
		eq([1,2].concat([2,1])[1], 2);
		eq([1,2].concat([2,1])[2], 2);
		eq([1,2].concat([2,1])[3], 1);

		// join
		eq([1,2].join(""), "12");
		eq([].join("x"), "");
		eq([1].join("x"), "1");
		eq([1,2].join("x"), "1x2");
		eq([].join(""), "");
		eq([new ClassWithToString(), new ClassWithToStringChild(), new ClassWithToStringChild2()].join("_"), "ClassWithToString.toString()_ClassWithToString.toString()_ClassWithToStringChild2.toString()");

		// pop
		eq([].pop(), null);
		eq([1].pop(), 1);
		var a = [1, 2, 3];
		var b = a;
		eq(a.pop(), 3);
		eq(a[0], 1);
		eq(a[1], 2);
		eq(a, b);
		eq(a.pop(), 2);
		eq(a[0], 1);
		eq(a, b);
		eq(a.pop(), 1);
		eq(a.length, 0);
		eq(a, b);
		eq(a.pop(), null);
		eq(a.length, 0);
		eq(a, b);

		// push
		var a:Array<Null<Int>> = [];
		var b = a;
		eq(a.push(1), 1);
		eq(a, b);
		eq(a[0], 1);
		eq(a.push(2), 2);
		eq(a, b);
		eq(a[0], 1);
		eq(a[1], 2);
		eq(a.push(null), 3);
		eq(a[0], 1);
		eq(a[1], 2);
		eq(a[2], null);

		// reverse
		var a = [1, 2, 3];
		var b = a;
		a.reverse();
		eq(a, b);
		eq(a[0], 3);
		eq(a[1], 2);
		eq(a[2], 1);
		var a = [];
		a.reverse();
		eq(a.length, 0);
		var a = [1];
		a.reverse();
		eq(a[0], 1);

		// shift
		eq([].shift(), null);
		eq([1].shift(), 1);
		var a = [1, 2, 3];
		var b = a;
		eq(a.shift(), 1);
		eq(a[0], 2);
		eq(a[1], 3);
		eq(a, b);
		eq(a.shift(), 2);
		eq(a[0], 3);
		eq(a, b);
		eq(a.shift(), 3);
		eq(a.length, 0);
		eq(a, b);
		eq(a.shift(), null);
		eq(a.length, 0);
		eq(a, b);

		// slice
		var i0 = new IntWrap(1);
		var i1 = new IntWrap(1);
		var i2 = new IntWrap(5);
		var i3 = new IntWrap(9);
		var i4 = new IntWrap(2);
		var a = [i4,i0,i1,i3,i0,i2];
		var b = a.slice(0);
		t(b != a);
		eq(b[0], i4);
		eq(b[1], i0);
		eq(b[2], i1);
		eq(b[3], i3);
		eq(b[4], i0);
		eq(b[5], i2);
		b = b.slice(1);
		eq(b[0], i0);
		eq(b[1], i1);
		eq(b[2], i3);
		eq(b[3], i0);
		eq(b[4], i2);
		b = b.slice(1, 3);
		eq(b[0], i1);
		eq(b[1], i3);
		b = b.slice( -1);
		eq(b[0], i3);
		b = b.slice(0, 4);
		eq(b[0], i3);
		eq(b.slice( -3)[0], i3);
		eq(b.slice( -3, -3).length, 0);
		eq([1, 2, 3].slice(2, 1).length, 0);

		// sort
		var i0 = new IntWrap(1);
		var i1 = new IntWrap(1);
		var i2 = new IntWrap(5);
		var i3 = new IntWrap(9);
		var i4 = new IntWrap(2);
		var a = [i4, i0, i1, i3, i0, i2];
		haxe.ds.ArraySort.sort(a, IntWrap.compare);
		eq(a[0], i0);
		eq(a[1], i1);
		eq(a[2], i0);
		eq(a[3], i4);
		eq(a[4], i2);
		eq(a[5], i3);

		// splice
		var i0 = new IntWrap(1);
		var i1 = new IntWrap(1);
		var i2 = new IntWrap(5);
		var i3 = new IntWrap(9);
		var i4 = new IntWrap(2);
		var b = [i4, i0, i1, i3, i0, i2];
		var a = b.splice(0, 0);
		t(b != a);
		eq(a.length, 0);
		eq(b[0], i4);
		eq(b[1], i0);
		eq(b[2], i1);
		eq(b[3], i3);
		eq(b[4], i0);
		eq(b[5], i2);
		a = b.splice(1, b.length - 1);
		eq(b[0], i4);
		eq(a[0], i0);
		eq(a[1], i1);
		eq(a[2], i3);
		eq(a[3], i0);
		eq(a[4], i2);
		b = a.splice(1, -1);
		eq(a[0], i0);
		eq(a[1], i1);
		eq(a[2], i3);
		eq(a[3], i0);
		eq(a[4], i2);
		eq(b.length, 0);
		b = a.splice(0, 10);
		eq(b[0], i0);
		eq(b[1], i1);
		eq(b[2], i3);
		eq(b[3], i0);
		eq(b[4], i2);
		eq(a.length, 0);
		a = b.splice(10, 10);
		eq(a.length, 0);
		b = [i0, i1, i3, i0, i2];
		a = b.splice( -2, 2);
		eq(b[0], i0);
		eq(b[1], i1);
		eq(b[2], i3);
		eq(a[0], i0);
		eq(a[1], i2);

		// toString
		var a = [new ClassWithToString(), new ClassWithToStringChild(), new ClassWithToStringChild2()];
		var comp = "ClassWithToString.toString(),ClassWithToString.toString(),ClassWithToStringChild2.toString()";
		t(a.toString() == comp || a.toString() == "[" + comp + "]");

		// unshift
		var a:Array<Null<Int>> = [];
		var b = a;
		a.unshift(1);
		eq(a, b);
		eq(a[0], 1);
		a.unshift(2);
		eq(a, b);
		eq(a[0], 2);
		eq(a[1], 1);
		a.unshift(null);
		eq(a[0], null);
		eq(a[1], 2);
		eq(a[2], 1);

		// insert
		var a = [];
		a.insert(5, 1);
		eq(a[0], 1);
		var a = [1, 2, 3];
		a.insert(1, 4);
		eq(a[0], 1);
		eq(a[1], 4);
		eq(a[2], 2);
		eq(a[3], 3);
		var a = [1, 2, 3];
		a.insert( -1, 4);
		eq(a[0], 1);
		eq(a[1], 2);
		eq(a[2], 4);
		eq(a[3], 3);
		a.insert( -2, 8);
		eq(a[0], 1);
		eq(a[1], 2);
		eq(a[2], 8);
		eq(a[3], 4);
		eq(a[4], 3);
		a.insert ( -8, 9);
		eq(a[0], 9);
		eq(a[1], 1);
		eq(a[2], 2);
		eq(a[3], 8);
		eq(a[4], 4);
		eq(a[5], 3);

		// remove
		var i0 = new IntWrap(1);
		var i1 = new IntWrap(1);
		var i2 = new IntWrap(5);
		var i3 = new IntWrap(9);
		var i4 = new IntWrap(2);
		var a = [i4, i0, i1, i3, i0, i2];
		t(a.remove(i0));
		eq(a[0], i4);
		eq(a[1], i1);
		eq(a[2], i3);
		eq(a[3], i0);
		eq(a[4], i2);
		t(a.remove(i0));
		eq(a[0], i4);
		eq(a[1], i1);
		eq(a[2], i3);
		eq(a[3], i2);
		f(a.remove(i0));
		eq(a[0], i4);
		eq(a[1], i1);
		eq(a[2], i3);
		eq(a[3], i2);
		var a = ["foo", "bar"];
		t(a.remove("foo"));
		eq(a[0], "bar");
		var a = [i0, null, i1, null, null];
		t(a.remove(null));
		eq(a[0], i0);
		eq(a[1], i1);
		eq(a[2], null);
		eq(a[3], null);
		t(a.remove(null));
		eq(a[0], i0);
		eq(a[1], i1);
		eq(a[2], null);
		t(a.remove(null));
		eq(a[0], i0);
		eq(a[1], i1);
		f(a.remove(null));
		eq(a[0], i0);
		eq(a[1], i1);

		// contains
		f([].contains(1));
		t([1].contains(1));
		f([1].contains(2));
		t([1,2].contains(1));
		t([1,2].contains(2));
		f([1,2].contains(3));
		#if !js // see https://github.com/HaxeFoundation/haxe/issues/3330
		t(([1,2]:Dynamic).contains(2));
		#end

		// indexOf
		eq([].indexOf(10), -1);
		eq([10].indexOf(10), 0);
		eq([10, 10].indexOf(10), 0);
		eq([2, 10].indexOf(10), 1);
		eq([2, 5].indexOf(10), -1);
		eq(["foo", "bar", "bar", "baz"].indexOf("bar"), 1);
		eq([1, 10, 10, 1].indexOf(10, 0), 1);
		eq([1, 10, 10, 1].indexOf(10, 1), 1);
		eq([1, 10, 10, 1].indexOf(10, 2), 2);
		eq([1, 10, 10, 1].indexOf(10, 3), -1);
		eq([1, 10, 10, 1].indexOf(10, 4), -1);
		eq([1, 10, 10, 1].indexOf(10, 5), -1);
		eq([1, 10, 10, 1].indexOf(10, -1), -1);
		eq([1, 10, 10, 1].indexOf(10, -2), 2);
		eq([1, 10, 10, 1].indexOf(10, -3), 1);
		eq([1, 10, 10, 1].indexOf(10, -5), 1);

		// lastIndexOf
		eq([].lastIndexOf(10), -1);
		eq([10].lastIndexOf(10), 0);
		eq([10, 10].lastIndexOf(10), 1);
		eq([2, 10].lastIndexOf(10), 1);
		eq([2, 5].lastIndexOf(10), -1);
		eq(["foo", "bar", "bar", "baz"].lastIndexOf("bar"), 2);
		eq([1, 10, 10, 1].lastIndexOf(10, 4), 2);
		eq([1, 10, 10, 1].lastIndexOf(10, 3), 2);
		eq([1, 10, 10, 1].lastIndexOf(10, 2), 2);
		eq([1, 10, 10, 1].lastIndexOf(10, 1), 1);
		eq([1, 10, 10, 1].lastIndexOf(10, 0), -1);
		eq([1, 10, 10, 1].lastIndexOf(10, -1), 2);
		eq([1, 10, 10, 1].lastIndexOf(10, -2), 2);
		eq([1, 10, 10, 1].lastIndexOf(10, -3), 1);
		eq([1, 10, 10, 1].lastIndexOf(10, -4), -1);
		eq([1, 10, 10, 1].lastIndexOf(10, -5), -1);

		// copy
		var i0 = new IntWrap(1);
		var i1 = new IntWrap(1);
		var i2 = new IntWrap(5);
		var a = [i0, i1, i2];
		var b = a.copy();
		t(a != b);
		eq(b[0], i0);
		eq(b[1], i1);
		eq(b[2], i2);
		var a = [];
		var b = a.copy();
		t(a != b);
		eq(b.length, 0);

		// map
		eq([1, 2, 3].map(function(i) return i * 2)[0], 2);
		eq([1, 2, 3].map(function(i) return i * 2)[1], 4);
		eq([1, 2, 3].map(function(i) return i * 2)[2], 6);
		var a = [new IntWrap(1), new IntWrap(2)];
		var b = a.map(function(x) return x);
		t(a != b);
		eq(b.length, a.length);
		eq(a[0], b[0]);
		eq(a[1], b[1]);
		var func = function(s) return s.toUpperCase();
		eq(["foo", "bar"].map(func)[0], "FOO");
		eq(["foo", "bar"].map(func)[1], "BAR");
		eq([].map(func).length, 0);

		// filter
		eq([1, 2, 3, 4].filter(function(i) return i < 3)[0], 1);
		eq([1, 2, 3, 4].filter(function(i) return i < 3)[1], 2);
		eq([1, 2, 3, 4].filter(function(i) return true)[0], 1);
		eq([1, 2, 3, 4].filter(function(i) return true)[1], 2);
		eq([1, 2, 3, 4].filter(function(i) return true)[2], 3);
		eq([1, 2, 3, 4].filter(function(i) return true)[3], 4);
		eq([1, 2, 3, 4].filter(function(i) return false).length, 0);
		eq([].filter(function(_) return true).length, 0);
		eq([].filter(function(_) return false).length, 0);
		var arr = [{id: 1}, {id: 2}, {id: 3}, {id: 4}, {id: 5}];
		arr = arr.filter(function(i) return i.id % 2 != 0);
		var values = [];
		for (a in arr) values.push(a.id);
		eq(values[0], 1);
		eq(values[1], 3);
		eq(values[2], 5);

		// check that map and filter work well on Dynamic as well
		var a : Dynamic = [0,1,2];
		var b : Dynamic = a.filter(function(x) return x & 1 == 0).map(function(x) return x * 10);
		eq(b.length, 2);
		eq(b[0], 0);
		eq(b[1], 20);

		// resize
		var a : Array<Int> = [1,2,3];
		a.resize(10);
		eq(a.length, 10);
		eq(a[0], 1);
		eq(a[1], 2);
		eq(a[2], 3);
		a.resize(2);
		eq(a.length, 2);
		eq(a[0], 1);
		eq(a[1], 2);
		a.resize(3);
		eq(a.length, 3);
		eq(a[0], 1);
		eq(a[1], 2);
		t(a[2] != 3);
		a.resize(0);
		eq(a.length, 0);
		eq(a.length, 0);

		// keyValueIterator
		var a : Array<Int> = [1,2,3,5,8];
		eq([for (k=>v in a) k][0], 0);
		eq([for (k=>v in a) k][1], 1);
		eq([for (k=>v in a) k][2], 2);
		eq([for (k=>v in a) k][3], 3);
		eq([for (k=>v in a) k][4], 4);
		eq([for (k=>v in a) v][0], 1);
		eq([for (k=>v in a) v][1], 2);
		eq([for (k=>v in a) v][2], 3);
		eq([for (k=>v in a) v][3], 5);
		eq([for (k=>v in a) v][4], 8);
		eq([for (k=>v in a) k*v][0], 0);
		eq([for (k=>v in a) k*v][1], 2);
		eq([for (k=>v in a) k*v][2], 6);
		eq([for (k=>v in a) k*v][3], 15);
		eq([for (k=>v in a) k*v][4], 32);

		// keyValueIterator through Structure
		var a : Array<Int> = [1,2,3,5,8];
		var it : KeyValueIterator<Int, Int> = a.keyValueIterator();
		var a2 = [for (k=>v in it) k];
		eq(a2[0], 0);
		eq(a2[1], 1);
		eq(a2[2], 2);
		eq(a2[3], 3);
		eq(a2[4], 4);
		var it : KeyValueIterator<Int, Int> = a.keyValueIterator();
		a2 = [for (k=>v in it) v];
		eq(a2[0], 1);
		eq(a2[1], 2);
		eq(a2[2], 3);
		eq(a2[3], 5);
		eq(a2[4], 8);
		var it : KeyValueIterator<Int, Int> = a.keyValueIterator();
		a2 = [for (k=>v in it) k*v];
		eq(a2[0], 0);
		eq(a2[1], 2);
		eq(a2[2], 6);
		eq(a2[3], 15);
		eq(a2[4], 32);

		// keyValueIterator through Structure
		var a : Array<Int> = [1,2,3,5,8];
		var it : KeyValueIterable<Int, Int> = a;
		eq([for (k=>v in it) k][0], 0);
		eq([for (k=>v in it) k][1], 1);
		eq([for (k=>v in it) k][2], 2);
		eq([for (k=>v in it) k][3], 3);
		eq([for (k=>v in it) k][4], 4);
		eq([for (k=>v in it) v][0], 1);
		eq([for (k=>v in it) v][1], 2);
		eq([for (k=>v in it) v][2], 3);
		eq([for (k=>v in it) v][3], 5);
		eq([for (k=>v in it) v][4], 8);
		eq([for (k=>v in it) k*v][0], 0);
		eq([for (k=>v in it) k*v][1], 2);
		eq([for (k=>v in it) k*v][2], 6);
		eq([for (k=>v in it) k*v][3], 15);
		eq([for (k=>v in it) k*v][4], 32);

		#if !flash
		// Can't create this closure on Flash apparently
		// keyValueIterator closure because why not
		var a : Array<Int> = [1,2,3,5,8];
		var itf : () -> KeyValueIterator<Int, Int> = a.keyValueIterator;
		var it = itf();
		var a2 = [for (k=>v in it) k];
		eq(a2[0], 0);
		eq(a2[1], 1);
		eq(a2[2], 2);
		eq(a2[3], 3);
		eq(a2[4], 4);
		var itf : () -> KeyValueIterator<Int, Int> = a.keyValueIterator;
		var it = itf();
		a2 = [for (k=>v in it) v];
		eq(a2[0], 1);
		eq(a2[1], 2);
		eq(a2[2], 3);
		eq(a2[3], 5);
		eq(a2[4], 8);
		var itf : () -> KeyValueIterator<Int, Int> = a.keyValueIterator;
		var it = itf();
		a2 = [for (k=>v in it) k*v];
		eq(a2[0], 0);
		eq(a2[1], 2);
		eq(a2[2], 6);
		eq(a2[3], 15);
		eq(a2[4], 32);
		#end
	}
}
