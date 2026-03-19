package unit.issues;

class Issue2871 extends Test {
	#if loose_numeric_casts
	function call(myUInt:Null<UInt> = null):Int {
		return myUInt == null ? 0 : myUInt;
	}

	function test() {
		eq(0, call(null));
		eq(1, call((1:UInt)));
	}
	#else
	function test() {
		noAssert();
	}
	#end
}