@:headerClassCode('
    static constexpr int64_t myInt64 = 10;
')
class Test {
	public function new() {}

	public function getInt64():cpp.Int64
		return untyped __cpp__('myInt64');
}

class Main {
	public static function main() {
		var t = new Test();

		#if (haxe > "4.2.5") // nightly rc

		// 1.
		var other:Int = t.getInt64(); //  Warning : (WDeprecated) Implicit cast from Int64 to Int (32 bits) is deprecated. Use .toInt() or explicitly cast instead.

		// 2. iterate implicitly truncated
		for (i in 0...t.getInt64()) // ERROR: cpp.Int64 should be Int
			trace(i);

		// 3. iterate explicitly truncated
		for (i in 0...t.getInt64().toInt()) // OK
			trace(i);

		// 4. iterate on int64-range
		var start:cpp.Int64 = 0;
		for (i in start...t.getInt64()) // ERROR: cpp.Int64 should be Int
			trace(i);
		#else // tested with 4.2.5

		// truncate implicitly
		var other:Int = t.getInt64(); // OK

		// iterate implicitly truncated
		for (i in 0...t.getInt64()) // OK
			trace(i);
		#end
	}
}
