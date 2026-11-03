package unit.issues;

class Issue11647 extends Test {
	#if (java || hl || cpp)
	function test() {
		foo();
	}

	function foo(a : Single = 10.0, b:Single = 11.0, c:Single = 12.0) : Void {
		feq(10, a);
		feq(11, b);
		feq(12, c);
	}
	#end
}
