class Main {
	// Not fully inferred at the end of their own typing.
	static var items = [];
	static var cache = null;
	static var table = new Map();
	static function unusedArg(x) return 1;
	static function callbackResult(f) return f();

	// Fully inferred.
	static var numbers = [1, 2];
	static var annotated:Array<String> = [];
	static function fact(n:Int) return n <= 1 ? 1 : n * fact(n - 1);
	static function generic<T>(v:T) return v;
	// The return type of a callback that appears nowhere else is Void.
	static function withCallback(f) { f(); }
	static function withOptionalCallback(?f) { if (f != null) f(1); }

	static function main() {
		new Impl().set(1);
		items.push(1);
		cache = "a";
		table.set("k", 1);
		withCallback(() -> {});
		withOptionalCallback();
		trace(items, cache, table, unusedArg(2), callbackResult(() -> 1), numbers, annotated, fact(3), generic(1));
	}
}

interface Setter {
	function set(bit:Int):Void;
}

class Impl implements Setter {
	public function new() {}

	// The argument has the type declared by the interface.
	public function set(_) {}
}
