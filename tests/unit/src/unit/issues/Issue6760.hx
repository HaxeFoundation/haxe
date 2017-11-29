package unit.issues;

class Issue6760 extends Test {

	function foo( x : UInt, ?opt : UInt ) : UInt {
		return opt == null ? x : opt;
	}

	function toString( ?v : UInt ) {
		return ""+v;
	}

	function test() {
		eq(foo(10), 10);
		eq(toString(), "null");
	}
}
