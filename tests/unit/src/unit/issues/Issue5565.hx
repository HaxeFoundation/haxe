package unit.issues;

class Issue5565 extends Test {
	#if php
	function test() {
		t(Std.is(php.Syntax.arrayDecl(), php.NativeArray));
	}
	#end
}