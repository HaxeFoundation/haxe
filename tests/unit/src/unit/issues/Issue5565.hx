package unit.issues;

class Issue5565 extends Test {
	function test() {
		#if php
        t(Std.is(untyped __php__('[]'), php.NativeArray));
        #end
	}
}