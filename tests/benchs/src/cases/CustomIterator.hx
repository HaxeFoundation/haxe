package cases;

import hxbenchmark.Suite;

class CustomIterator<T> extends TestCase {

	var MAX_TIME_PER_CASE:Float = .5;
	var N = 1000;

	function measureInt() {
		var data = [for (i in 0...N) i];
		var custom = new Custom(data);

		var suite = new Suite('Custom(Array<Int>[$N])', MAX_TIME_PER_CASE);
		suite.add("inlineIterator()", for (v in custom.inlineIterator()) {});
		suite.add("iterator()", for (v in custom.iterator()) {});
		suite.add("iteratorT()", for (v in custom.iteratorT()) {});
		suite.add("iter(func)", custom.iter(function(v) {}));
		return suite.run();
	}

	function measureString() {
		var data = [for (i in 0...N) "" + i];
		var custom = new Custom(data);

		var suite = new Suite('Custom(Array<String>[$N])', MAX_TIME_PER_CASE);
		suite.add("inlineIterator()", for (v in custom.inlineIterator()) {});
		suite.add("iterator()", for (v in custom.iterator()) {});
		suite.add("iteratorT()", for (v in custom.iteratorT()) {});
		suite.add("iter(func)", custom.iter(function(v) {}));
		return suite.run();
	}

	function measureDummy() {
		var data = [for (i in 0...N) new Dummy()];
		var custom = new Custom(data);

		var suite = new Suite('Custom(Array<Dummy>[$N])', MAX_TIME_PER_CASE);
		suite.add("inlineIterator()", for (v in custom.inlineIterator()) {});
		suite.add("iterator()", for (v in custom.iterator()) {});
		suite.add("iteratorT()", for (v in custom.iteratorT()) {});
		suite.add("iter(func)", custom.iter(function(v) {}));
		return suite.run();
	}
}

class Custom<T> {
	public var data:Array<T>;

	public function new(data:Array<T>) {
		this.data = data;
	}

	inline public function inlineIterator():Iterator<T> {
		return data.iterator();
	}

	public function iterator():Iterator<T> {
		return data.iterator();
	}

	public function iteratorT():Iterator<T> {
		return new IteratorT(this);
	}

	public function iter(func:T->Void) {
		var len = data.length;
		var idx = 0;
		while (idx < len) {
			func(data[idx]);
			idx++;
		}
	}
}

class IteratorT<T> {
	var data:Array<T>;
	var len:Int;
	var idx:Int;

	public function new(custom:Custom<T>) {
		this.data = custom.data;
		this.len = data.length;
		this.idx = 0;
	}

	inline public function hasNext():Bool {
		return idx < len;
	}

	inline public function next():T {
		return data[idx++];
	}
}

class Dummy {
	var x:Int;
	var s:String;
	var o:{};

	public function new() {}
}