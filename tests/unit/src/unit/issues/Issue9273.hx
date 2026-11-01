package unit.issues;

class Issue9273 extends unit.Test {
#if flash_test_swc
	function test() {
		eq("hello", new HaxeExtendsSwc().strField);
	}
#end
}

#if flash_test_swc
private class HaxeExtendsSwc extends ParentCtorWithDefaultStringArgument {}
#end