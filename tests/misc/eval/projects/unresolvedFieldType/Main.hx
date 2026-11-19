class Main {
	// Not fully inferred at the end of their own typing.
	static var items = [];
	static var cache = null;
	static var table = new Map();
	static function unusedArg(x) return 1;

	// Fully inferred.
	static var numbers = [1, 2];
	static var annotated:Array<String> = [];
	static function fact(n:Int) return n <= 1 ? 1 : n * fact(n - 1);
	static function generic<T>(v:T) return v;

	static function main() {
		items.push(1);
		cache = "a";
		table.set("k", 1);
		trace(items, cache, table, unusedArg(2), numbers, annotated, fact(3), generic(1));
	}
}
