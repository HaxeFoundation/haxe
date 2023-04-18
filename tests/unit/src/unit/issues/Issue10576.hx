package unit.issues;

class Issue10576 extends Test {
#if php
	function test() {
		cast(new php.NativeArray(), php.NativeArray);
        noAssert();
	}
#end
}
