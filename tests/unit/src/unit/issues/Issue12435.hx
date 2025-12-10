package unit.issues;

class Issue12435 extends Test {
	function test() {
		var i:Int = cast null;
		#if static
		t(i == 0);
		#else
		f(i == 0);
		t(i == null);
		#end
	}
}
