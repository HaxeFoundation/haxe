package unit.issues;

class Issue10943 extends Test {

	function test() {
		#if (!php && !python)
		var x:Int = 1<<32;
		eq(1, x);
		var n:Int = 1;
		var m:Int = n << 32;
		eq(1, m);
		#end
		
		var x:Int32 = 1<<32;
		eq(1, x);
		var n:Int32 = 1;
		var m:Int32 = n << 32;
		eq(1, m);
	}

}
