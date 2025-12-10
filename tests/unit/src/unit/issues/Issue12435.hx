package unit.issues;

class Issue12435 extends Test {
	function testCast() {
		var i:Int = cast null;
		#if static
		t(i == 0);
		t(i == cast null);
		#else
		f(i == 0);
		t(i == null);
		#end
	}

	function testPropagation() {
		var Null:Null<Int> = null;
		var i = Null;
		#if static
		t(i == 0);
		t(i == Null);
		#else
		f(i == 0);
		t(i == null);
		#end
	}
}
