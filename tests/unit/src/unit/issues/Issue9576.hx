package unit.issues;

class Issue9576 extends Test {
	#if jvm
	function test() {
		eq(6, foo(cast i -> i * 2));
	}

	function foo(f:java.util.function.Function<Int, Int>) {
		return f.apply(3);
	}
	#end
}
