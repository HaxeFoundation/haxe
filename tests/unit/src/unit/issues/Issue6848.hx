package unit.issues;

class Issue6848 extends unit.Test {
#if php
	function test() {
		var e = Type.createInstance(php.Exception, ['hello']);
		t(Std.isOfType(e, php.Exception));
		eq('hello', e.getMessage());
	}
#end
}