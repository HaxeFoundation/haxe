package unit.issues;

class Issue9273 extends unit.Test {
#if flash
	function test() {
		eq("hello", new HaxeExtendsSwc().strField);
	}
#end
}

#if flash
private class HaxeExtendsSwc extends ParentCtorWithDefaultStringArgument {}
#end