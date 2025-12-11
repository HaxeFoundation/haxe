package unit.issues;

class Issue12435 extends Test {
	function testCast() {
		var i:Int = cast null;
		#if static
		t(i == 0);
		#if !cppia // TODO
		t(i == cast null);
		#end
		#else
		f(i == 0);
		t(i == null);
		#end
	}

	function testPropagation() {
		var Null:Null<Int> = null;
		var i:Int = Null;
		#if static
		t(i == 0);
		f(i == Null);
		#else
		f(i == 0);
		t(i == null);
		#end
	}

	function testPropagationNullable() {
		var Null:Null<Int> = null;
		var i:Null<Int> = Null;
		f(i == 0);
		t(i == null);
	}
}
