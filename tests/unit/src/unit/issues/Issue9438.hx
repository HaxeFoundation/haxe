package unit.issues;

class Issue9438 extends unit.Test {
	#if jvm
	function test() {
		var x:java.util.Set = null;
		utest.Assert.pass();
	}
	#end
}
