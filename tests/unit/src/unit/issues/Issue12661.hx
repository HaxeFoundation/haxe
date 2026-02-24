package unit.issues;

class Issue12661 extends Test {
	// Overloads differing only in callback return type (Void vs. String)
	extern inline overload static function check(f:() -> Void):Int return 1;
	extern inline overload static function check(f:() -> String):Int return 2;

	// Overloads differing in callback return type (Int vs. String)
	extern inline overload static function check2(f:() -> Int):Int return f();
	extern inline overload static function check2(f:() -> String):String return f();

	function test() {
		// Arrow function with block-throw body should prefer Void overload
		eq(1, check(() -> { throw "oh no"; }));

		// Inline throw (no block) should prefer Void overload
		eq(1, check(() -> throw "oh no"));

		// Non-arrow function with throw-only body should prefer Void overload
		eq(1, check(function() { throw "oh no"; }));

		// Throw inside an if-else (all branches throw) should prefer Void overload
		var x = true;
		eq(1, check(() -> { if (x) throw "a"; else throw "b"; }));

		// Throw inside an infinite while loop should prefer Void overload
		eq(1, check(() -> { while (true) throw "loop"; }));

		// String literal return should prefer String overload over Void
		eq(2, check(() -> "foo"));

		// Block with String value should prefer String overload over Void
		eq(2, check(() -> { var s = "bar"; s; }));

		// Normal overload resolution still works: concrete return types disambiguate
		eq(42, check2(() -> 42));
		eq("hi", check2(() -> "hi"));
	}
}
