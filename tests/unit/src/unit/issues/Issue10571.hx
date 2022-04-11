package unit.issues;

class Issue10571 extends Test {
	#if java
	function test() {
		eq("test()", foo());
		eq("test(I)", foo(1));
	}

	overload static function foo() {
		function x() {
			return "test()";
		}
		return x();
	}

	overload static function foo(i:Int) {
		function x() {
			return "test(I)";
		}
		return x();
	}
	#end
}
