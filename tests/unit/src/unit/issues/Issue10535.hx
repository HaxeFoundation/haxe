package unit.issues;

class Issue10535 extends Test {
	#if hl
	function foo<T>(a:Array<T>) {}

	function test() {
		var x:hl.F32 = 10.0;
		foo([x]);
		utest.Assert.pass();
	}
	#end
}
