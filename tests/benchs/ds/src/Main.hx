import haxe.ds.Vector;
import thx.benchmark.speed.Suite;

class Main {
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
		var suite = new Suite();
		suite.add("Vector create", () -> @:measure new Vector(0));
		suite.add("Array create", () -> @:measure new Array());
		suite.add("StringMap create", () -> @:measure new haxe.ds.StringMap());
		suite.add("IntMap create", () -> @:measure new haxe.ds.IntMap());
		suite.add("ObjectMap create", () -> @:measure new haxe.ds.ObjectMap());
		var stats = suite.run();
		trace(stats.toString());
	}

	@:analyzer(ignore)
	static function measureRead() {
		var suite = new Suite();
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector read", () -> @:measure v[0]);
		suite.add("Array read", () -> @:measure a[0]);
		suite.add("StringMap read", () -> @:measure sm["foo"]);
		suite.add("IntMap read", () -> @:measure im[1]);
		suite.add("ObjectMap read", () -> @:measure om[key]);
		var stats = suite.run();
		trace(stats.toString());
	}

	static function measureWrite() {
		var suite = new Suite();
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector write", () -> @:measure v[0] = 1);
		suite.add("Array write", () -> @:measure a[0] = 1);
		suite.add("StringMap write", () -> @:measure sm["foo"] = 1);
		suite.add("IntMap write", () -> @:measure im[1] = 1);
		suite.add("ObjectMap write", () -> @:measure om[key] = 1);
		var stats = suite.run();
		trace(stats.toString());
	}

	static function measureReadWrite() {
		var suite = new Suite();
		var a = [1];
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector read-write", () -> @:measure v[0] += 1);
		suite.add("Array read-write", () -> @:measure a[0] += 1);
		suite.add("StringMap read-write", () -> @:measure sm["foo"] += 1);
		suite.add("IntMap read-write", () -> @:measure im[1] += 1);
		suite.add("ObjectMap read-write", () -> @:measure om[key] += 1);
		var stats = suite.run();
		trace(stats.toString());
	}

	static function measureIterate() {
		var suite = new Suite();
		var a = [];
		for (i in 0...10000) a[i] = i;
		var v = Vector.fromArrayCopy(a);
		var sm = new Map();
		var im = new Map();
		var om = new Map();
		var key = { foo: 1 };
		suite.add("Vector iterate + write", () -> @:measure for (i in v) { v[i] = i; });
		suite.add("Array iterate + write", () -> @:measure for (i in a) { a[i] = i; });
		suite.add("StringMap iterate + write", () -> @:measure for (i in a) { sm["foo"] = i; });
		suite.add("IntMap iterate + write", () -> @:measure for (i in a) { im[1] = i; });
		suite.add("ObjectMap iterate + write", () -> @:measure for (i in a) { om[key] = i; });
		var stats = suite.run();
		trace(stats.toString());
	}

	static function measureMap() {
		var suite = new Suite();
		var a = [];
		for (i in 0...10000) a[i] = i;
		var v = Vector.fromArrayCopy(a);
		function f(x) return x;
		suite.add("Vector map", () -> @:measure v.map(f));
		suite.add("Array map", () -> @:measure a.map(f));
		var stats = suite.run();
		trace(stats.toString());
	}

	static function measureJoin() {
		var suite = new Suite();
		var a = [];
		for (i in 0...10000) a[i] = i;
		var v = Vector.fromArrayCopy(a);
		function f(x) return x;
		suite.add("Vector join", () -> @:measure v.join(""));
		suite.add("Array join", () -> @:measure a.join(""));
		var stats = suite.run();
		trace(stats.toString());
	}

	static function measureCopy() {
		var suite = new Suite();
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
		suite.add("Vector copy", () -> @:measure v.copy());
		suite.add("Array copy", () -> @:measure a.copy());
		suite.add("StringMap copy", () -> @:measure sm.copy());
		suite.add("IntMap copy", () -> @:measure im.copy());
		suite.add("ObjectMap copy", () -> @:measure om.copy());
		var stats = suite.run();
		trace(stats.toString());
	}
}