package unit.issues;

class Issue5565 extends Test {
	function test() {
		#if php
		t(Std.is(php.Syntax.arrayDecl(), php.NativeArray));
		#end
	}
}