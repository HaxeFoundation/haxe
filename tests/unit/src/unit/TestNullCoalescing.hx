package unit;

@:nullSafety(StrictThreaded)
class TestNullCoalescing extends Test {
	final nullInt:Null<Int> = null;
	final nullBool:Null<Bool> = null;

	function test() {
		eq(null ?? nullInt, null);
		eq(null ?? nullBool, null);
		final a = Std.random(0) + 1;
		final b = Std.random(0) + 2;
		eq(1 + a + 1 ?? 1 + b + 1, 3);

		final nullableBool:Null<Bool> = false;
		final testBool:Bool = nullBool ?? true;
		final testNullBool = null ?? nullableBool;
		final s:Int = nullInt == null ? 2 : nullInt;
		final s:Int = if (nullInt == null) 2; else nullInt;
		final s:Int = nullInt ?? 2;

		// $type(testBool); // Bool
		// $type(testNullBool); // Null<Bool>
		// $type(s); // Int

		eq(testBool, true);
		eq(testNullBool, false);
		eq(s, 2);

		eq(nullInt ?? 2 ?? 3 + 100, 2);
		eq(nullInt ?? nullInt ?? 3, 3);
	}
}
