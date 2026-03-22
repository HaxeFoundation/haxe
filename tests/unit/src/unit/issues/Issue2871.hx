package unit.issues;

class Issue2871 extends Test {
	function call(myUInt:Null<UInt> = null):Int {
		return myUInt == null ? 0 : myUInt.toInt();
	}

	function test() {
		eq(0, call(null));
		eq(1, call((1 : UInt)));
	}
}
