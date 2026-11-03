package issues;

private class MyIter {
	var i:Int;
	var n:Int;

	public inline function new(n:Int) {
		this.i = 0;
		this.n = n;
	}

	public inline function hasNext()
		return i < n;

	public inline function next()
		return i++;
}

private class Coll {
	public inline function new() {}

	public inline function iterator():Iterator<Int>
		return new MyIter(3);
}

class Issue7379 {
	@:js('
		var _g_i = 0;
		while(_g_i < 3) issues_Issue7379.use(_g_i++);
	')
	static function test() {
		var c = new Coll();
		for (value in c) {
			use(value);
		}
	}

	@:pure(false) static function use(v:Int) {}
}
