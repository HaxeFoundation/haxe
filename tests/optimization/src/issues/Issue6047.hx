package issues;

class Issue6047 {
	static inline function foo (x:Int, f:Int->Int) {
		return f(x);
	};

	static inline function plus2 (x:Int) return x + 2;

	@:js('
		issues_Issue6047.call(3);
		issues_Issue6047.call(3);
	')
	static function test() {
		var r1 = foo(1, plus2); // plus2 is not inlined
		var r2 = foo(1, function (i) return plus2(i)); // plus2 gets inlined when wrapped in anon function

		call(r1);
		call(r2);
	}

	@:pure(false) static function call(d) { }
}