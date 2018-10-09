package cases;

import haxe.ds.Vector;
import hxbenchmark.Suite;

class Ds extends TestCase {
	function measureCreate() {
		var suite = new Suite("create");
		suite.add("Vector", new Vector(0));
		suite.add("Array", new Array());
		suite.add("StringMap", new haxe.ds.StringMap());
		suite.add("IntMap", new haxe.ds.IntMap());
		suite.add("ObjectMap", new haxe.ds.ObjectMap());
		return suite.run();
	}

	@:analyzer(ignore)
	function measureRead() {
		var suite = new Suite("read");
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector", v[0]);
		suite.add("Array", a[0]);
		suite.add("StringMap", sm["foo"]);
		suite.add("IntMap", im[1]);
		suite.add("ObjectMap", om[key]);
		return suite.run();
	}

	function measureWrite() {
		var suite = new Suite("write");
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector", v[0] = 1);
		suite.add("Array", a[0] = 1);
		suite.add("StringMap", sm["foo"] = 1);
		suite.add("IntMap", im[1] = 1);
		suite.add("ObjectMap", om[key] = 1);
		return suite.run();
	}

	function measureReadWrite() {
		var suite = new Suite("read + write");
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = ["foo" => 0];
		var im = [1 => 0];
		var key = { foo: 1 };
		var om = [key => 0];
		suite.add("Vector", v[0] += 1);
		suite.add("Array", a[0] += 1);
		suite.add("StringMap", sm["foo"] += 1);
		suite.add("IntMap", im[1] += 1);
		suite.add("ObjectMap", om[key] += 1);
		return suite.run();
	}

	function measureIterate() {
		var suite = new Suite("iterate + write");
		var a = [];
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		for (i in 0...10000) {
			a[i] = i;
			im[i] = i;
			sm["" + i] = i;
			om[{foo: i}] = i;
		}
		var v = Vector.fromArrayCopy(a);
		var key = { foo: 1 };
		suite.add("Vector", for (i in v) { v[i] = i; });
		suite.add("Array", for (i in a) { a[i] = i; });
		suite.add("StringMap", for (i in a) { sm["foo"] = i; });
		suite.add("IntMap", for (i in a) { im[1] = i; });
		suite.add("ObjectMap", for (i in a) { om[key] = i; });
		return suite.run();
	}

	function measureMap() {
		var suite = new Suite("map");
		var a = [];
		for (i in 0...10000) a[i] = i;
		var v = Vector.fromArrayCopy(a);
		function f(x) return x;
		suite.add("Vector", v.map(f));
		suite.add("Array", a.map(f));
		return suite.run();
	}

	function measureJoin() {
		var suite = new Suite("join");
		var a = [];
		for (i in 0...10000) a[i] = i;
		var v = Vector.fromArrayCopy(a);
		function f(x) return x;
		suite.add("Vector", v.join(""));
		suite.add("Array", a.join(""));
		return suite.run();
	}

	function measureCopy() {
		var suite = new Suite("copy");
		var a = [];
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		for (i in 0...10000) {
			sm["" + i] = i;
			im[i] = i;
			om[{key: i}] = i;
			a[i] = i;
		}
		var v = Vector.fromArrayCopy(a);
		function f(x) return x;
		suite.add("Vector", v.copy());
		suite.add("Array", a.copy());
		suite.add("StringMap", sm.copy());
		suite.add("IntMap", im.copy());
		suite.add("ObjectMap", om.copy());
		return suite.run();
	}
}