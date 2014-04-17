package unit.issues;

class Issue2871 extends Test {
	#if !java
    function call(myUInt:Null<UInt> = null) {
        return myUInt == null ? 0 : myUInt;
    }

	function test() {
		eq(0, call(null));
		eq(1, call((1:UInt)));
	}
	#end
}