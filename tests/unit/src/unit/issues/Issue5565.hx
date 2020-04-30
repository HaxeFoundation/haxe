package unit.issues;

class Issue5565 extends Test {
	#if php
	function test() {
		t(Std.isOfType(php.Syntax.arrayDecl(), php.NativeArray));
	}
	#end
}