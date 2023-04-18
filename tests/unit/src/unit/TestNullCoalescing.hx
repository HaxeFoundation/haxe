package unit;

@:nullSafety(StrictThreaded)
class TestNullCoalescing extends Test {
	final nullInt:Null<Int> = null;
	final nullBool:Null<Bool> = null;
	final nullString:Null<String> = null;

	var count = 0;
	function call() {
		count++;
		return "_";
	}

	function test() {
		var a = call() ?? "default";
		eq(count, 1);

		eq(nullInt ?? nullInt, null);
		eq(nullBool ?? nullBool, null);

		final a:Dynamic = Std.random(0) + 1;
		final b = Std.random(0) + 2;
		eq(1 + a + 1 ?? 1 + b + 1, 3);

		final nullableBool:Null<Bool> = false;
		final testBool = nullBool ?? true;
		final testNullBool = nullBool ?? nullableBool;
		final s:Int = nullInt == null ? 2 : nullInt;
		final s:Int = if (nullInt == null) 2; else nullInt;
		final s = nullInt ?? 2;

		// $type(testBool); // Bool
		// $type(testNullBool); // Null<Bool>
		// $type(s); // Int
		final shouldBeBool:Bool = testBool;
		if (testNullBool == null) {}
		final shouldBeInt:Int = s;

		eq(testBool, true);
		eq(testNullBool, false);
		eq(s, 2);

		eq(nullInt == null ? 2 : nullInt, 2);
		eq(nullInt ?? 2, 2);
		eq(nullInt ?? (2 : Null<Int>) ?? 3 + 100, 2);
		eq(nullInt ?? nullInt ?? 3, 3);

		final i:Null<Int> = 1;
		final arr:Array<Int> = [i ?? 2];
		arr.push(i ?? 2);
		arr.push((1 : Null<Int>) ?? 2);
		eq(arr[0], 1);
		eq(arr[1], 1);
		eq(arr[2], 1);

		final arr = [
			nullInt ?? 2,
			2
		];
		eq(arr[0], arr[1]);

		var a = [0 => nullInt ?? 0 + 100];
		eq(a[0], 100);

		final di:Null<Dynamic> = null;
		final di2:Null<Dynamic> = null;
		final di3:Null<Dynamic> = 2;
		eq(di ?? di2 ?? di3, 2);

		var a:Null<Int> = null;
		a ??= 5;
		eq(a, 5);
		var a:Null<Int> = null;
		eq(a ??= 5, 5);
		eq(a, 5);
		var a = "default";
		eq(a ??= "5", "default");

		count = 0;
		var a = call();
		eq(count, 1);
		a ??= call();
		eq(count, 1);

		var a:Null<String> = null;
		final b = a ??= call();
		final c = a ??= call();
		eq(count, 2);
		eq(a, "_");
		eq(b, "_");
		eq(c, "_");

		final a:Null<Int> = ({} : Dynamic).x;
		eq(a ?? 2, 2);

		final a = nullInt;
		eq(a ?? 2, 2);

		final a = nullString;
		eq(a ?? "2", "2");

		eq(1 ?? 2, 1);
		eq("1" ?? "2", "1");

		final arr = [];
		function item(n) {
			arr.push(n);
			return n;
		}
		eq(item(1) ?? item(2) ?? item(3), 1);
		eq(arr.length, 1);
		for (i => v in [1]) eq(arr[i], v);

		final arr = [];
		function item(n) {
			arr.push(n);
			return null;
		}
		eq(item(1) ?? item(2) ?? item(3), null);
		eq(arr.length, 3);
		for (i => v in [1, 2, 3]) eq(arr[i], v);
	}
}
