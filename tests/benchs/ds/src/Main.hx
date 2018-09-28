import haxe.ds.Vector;

import hxbenchmark.Suite;

class Main {
	static var printer = new hxbenchmark.ResultPrinter();

	static function main() {
		measureCreate();
		measureRead();
		measureWrite();
		measureReadWrite();
		measureIterate();
		measureMap();
		measureJoin();
		measureCopy();
	}

	static function measureCreate() {
		var suite = new Suite("create");
		suite.add("Vector create", new Vector(0));
		suite.add("Array create", new Array());
		suite.add("StringMap create", new haxe.ds.StringMap());
		suite.add("IntMap create", new haxe.ds.IntMap());
		suite.add("ObjectMap create", new haxe.ds.ObjectMap());
		var stats = suite.run();
		trace(printer.print(stats));
	}

	@:analyzer(ignore)
	static function measureRead() {
		var suite = new Suite("read");
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector read", v[0]);
		suite.add("Array read", a[0]);
		suite.add("StringMap read", sm["foo"]);
		suite.add("IntMap read", im[1]);
		suite.add("ObjectMap read", om[key]);
		var stats = suite.run();
		trace(printer.print(stats));
	}

	static function measureWrite() {
		var suite = new Suite("write");
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector write", v[0] = 1);
		suite.add("Array write", a[0] = 1);
		suite.add("StringMap write", sm["foo"] = 1);
		suite.add("IntMap write", im[1] = 1);
		suite.add("ObjectMap write", om[key] = 1);
		var stats = suite.run();
		trace(printer.print(stats));
	}

	static function measureReadWrite() {
		var suite = new Suite("write");
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector read-write", v[0] += 1);
		suite.add("Array read-write", a[0] += 1);
		suite.add("StringMap read-write", sm["foo"] += 1);
		suite.add("IntMap read-write", im[1] += 1);
		suite.add("ObjectMap read-write", om[key] += 1);
		var stats = suite.run();
		trace(printer.print(stats));
	}

	static function measureIterate() {
		var suite = new Suite("suite");
		var a = [];
		for (i in 0...10000) a[i] = i;
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector iterate + write", for (i in v) { v[i] = i; });
		suite.add("Array iterate + write", for (i in a) { a[i] = i; });
		suite.add("StringMap iterate + write", for (i in a) { sm["foo"] = i; });
		suite.add("IntMap iterate + write", for (i in a) { im[1] = i; });
		suite.add("ObjectMap iterate + write", for (i in a) { om[key] = i; });
		var stats = suite.run();
		trace(printer.print(stats));
	}

	static function measureMap() {
		var suite = new Suite("map");
		var a = [];
		for (i in 0...10000) a[i] = i;
		var v = Vector.fromArrayCopy(a);
		function f(x) return x;
		suite.add("Vector map", v.map(f));
		suite.add("Array map", a.map(f));
		var stats = suite.run();
		trace(printer.print(stats));
	}

	static function measureJoin() {
		var suite = new Suite("join");
		var a = [];
		for (i in 0...10000) a[i] = i;
		var v = Vector.fromArrayCopy(a);
		function f(x) return x;
		suite.add("Vector join", v.join(""));
		suite.add("Array join", a.join(""));
		var stats = suite.run();
		trace(printer.print(stats));
	}

	static function measureCopy() {
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
		suite.add("Vector copy", v.copy());
		suite.add("Array copy", a.copy());
		suite.add("StringMap copy", sm.copy());
		suite.add("IntMap copy", im.copy());
		suite.add("ObjectMap copy", om.copy());
		var stats = suite.run();
		trace(printer.print(stats));
	}
}