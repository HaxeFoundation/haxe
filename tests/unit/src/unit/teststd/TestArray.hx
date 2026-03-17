package unit.teststd;

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
		aeq([1, 2], [1].concat([2]));
		aeq([1, 2, 2, 1], [1,2].concat([2,1]));

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
		aeq([1, 2], a);
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
		aeq([1, 2], a);
		eq(a.push(null), 3);
		aeq([1, 2, null], a);

		// reverse
		var a = [1, 2, 3];
		var b = a;
		a.reverse();
		eq(a, b);
		aeq([3, 2, 1], a);
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
		aeq([2, 3], a);
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
		aeq([i4, i0, i1, i3, i0, i2], b);
		b = b.slice(1);
		aeq([i0, i1, i3, i0, i2], b);
		b = b.slice(1, 3);
		aeq([i1, i3], b);
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
		aeq([i0, i1, i0, i4, i2, i3], a);

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
		aeq([i4, i0, i1, i3, i0, i2], b);
		a = b.splice(1, b.length - 1);
		eq(b[0], i4);
		aeq([i0, i1, i3, i0, i2], a);
		b = a.splice(1, -1);
		aeq([i0, i1, i3, i0, i2], a);
		eq(b.length, 0);
		b = a.splice(0, 10);
		aeq([i0, i1, i3, i0, i2], b);
		eq(a.length, 0);
		a = b.splice(10, 10);
		eq(a.length, 0);
		b = [i0, i1, i3, i0, i2];
		a = b.splice( -2, 2);
		aeq([i0, i1, i3], b);
		aeq([i0, i2], a);

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
		aeq([2, 1], a);
		a.unshift(null);
		aeq([null, 2, 1], a);

		// insert
		var a = [];
		a.insert(5, 1);
		eq(a[0], 1);
		var a = [1, 2, 3];
		a.insert(1, 4);
		aeq([1, 4, 2, 3], a);
		var a = [1, 2, 3];
		a.insert( -1, 4);
		aeq([1, 2, 4, 3], a);
		a.insert( -2, 8);
		aeq([1, 2, 8, 4, 3], a);
		a.insert ( -8, 9);
		aeq([9, 1, 2, 8, 4, 3], a);

		// remove
		var i0 = new IntWrap(1);
		var i1 = new IntWrap(1);
		var i2 = new IntWrap(5);
		var i3 = new IntWrap(9);
		var i4 = new IntWrap(2);
		var a = [i4, i0, i1, i3, i0, i2];
		t(a.remove(i0));
		aeq([i4, i1, i3, i0, i2], a);
		t(a.remove(i0));
		aeq([i4, i1, i3, i2], a);
		f(a.remove(i0));
		aeq([i4, i1, i3, i2], a);
		var a = ["foo", "bar"];
		t(a.remove("foo"));
		eq(a[0], "bar");
		var a = [i0, null, i1, null, null];
		t(a.remove(null));
		aeq([i0, i1, null, null], a);
		t(a.remove(null));
		aeq([i0, i1, null], a);
		t(a.remove(null));
		aeq([i0, i1], a);
		f(a.remove(null));
		aeq([i0, i1], a);

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
		aeq([i0, i1, i2], b);
		var a = [];
		var b = a.copy();
		t(a != b);
		eq(b.length, 0);

		// map
		aeq([2, 4, 6], [1, 2, 3].map(function(i) return i * 2));
		var a = [new IntWrap(1), new IntWrap(2)];
		var b = a.map(function(x) return x);
		t(a != b);
		eq(b.length, a.length);
		aeq([b[0], b[1]], a);
		var func = function(s) return s.toUpperCase();
		aeq(["FOO", "BAR"], ["foo", "bar"].map(func));
		eq([].map(func).length, 0);

		// filter
		aeq([1, 2], [1, 2, 3, 4].filter(function(i) return i < 3));
		aeq([1, 2, 3, 4], [1, 2, 3, 4].filter(function(i) return true));
		eq([1, 2, 3, 4].filter(function(i) return false).length, 0);
		eq([].filter(function(_) return true).length, 0);
		eq([].filter(function(_) return false).length, 0);
		var arr = [{id: 1}, {id: 2}, {id: 3}, {id: 4}, {id: 5}];
		arr = arr.filter(function(i) return i.id % 2 != 0);
		var values = [];
		for (a in arr) values.push(a.id);
		aeq([1, 3, 5], values);

		// check that map and filter work well on Dynamic as well
		var a : Dynamic = [0,1,2];
		var b : Dynamic = a.filter(function(x) return x & 1 == 0).map(function(x) return x * 10);
		eq(b.length, 2);
		aeq([0, 20], b);

		// resize
		var a : Array<Int> = [1,2,3];
		a.resize(10);
		eq(a.length, 10);
		eq(a[0], 1); eq(a[1], 2); eq(a[2], 3);
		a.resize(2);
		eq(a.length, 2);
		aeq([1, 2], a);
		a.resize(3);
		eq(a.length, 3);
		eq(a[0], 1); eq(a[1], 2);
		t(a[2] != 3);
		a.resize(0);
		eq(a.length, 0);
		eq(a.length, 0);

		// keyValueIterator
		var a : Array<Int> = [1,2,3,5,8];
		aeq([0, 1, 2, 3, 4], [for (k=>v in a) k]);
		aeq([1, 2, 3, 5, 8], [for (k=>v in a) v]);
		aeq([0, 2, 6, 15, 32], [for (k=>v in a) k*v]);

		// keyValueIterator through Structure
		var a : Array<Int> = [1,2,3,5,8];
		var it : KeyValueIterator<Int, Int> = a.keyValueIterator();
		var a2 = [for (k=>v in it) k];
		aeq([0, 1, 2, 3, 4], a2);
		var it : KeyValueIterator<Int, Int> = a.keyValueIterator();
		a2 = [for (k=>v in it) v];
		aeq([1, 2, 3, 5, 8], a2);
		var it : KeyValueIterator<Int, Int> = a.keyValueIterator();
		a2 = [for (k=>v in it) k*v];
		aeq([0, 2, 6, 15, 32], a2);

		// keyValueIterator through Structure
		var a : Array<Int> = [1,2,3,5,8];
		var it : KeyValueIterable<Int, Int> = a;
		aeq([0, 1, 2, 3, 4], [for (k=>v in it) k]);
		aeq([1, 2, 3, 5, 8], [for (k=>v in it) v]);
		aeq([0, 2, 6, 15, 32], [for (k=>v in it) k*v]);

		#if !flash
		// Can't create this closure on Flash apparently
		// keyValueIterator closure because why not
		var a : Array<Int> = [1,2,3,5,8];
		var itf : () -> KeyValueIterator<Int, Int> = a.keyValueIterator;
		var it = itf();
		var a2 = [for (k=>v in it) k];
		aeq([0, 1, 2, 3, 4], a2);
		var itf : () -> KeyValueIterator<Int, Int> = a.keyValueIterator;
		var it = itf();
		a2 = [for (k=>v in it) v];
		aeq([1, 2, 3, 5, 8], a2);
		var itf : () -> KeyValueIterator<Int, Int> = a.keyValueIterator;
		var it = itf();
		a2 = [for (k=>v in it) k*v];
		aeq([0, 2, 6, 15, 32], a2);
		#end
	}
}
