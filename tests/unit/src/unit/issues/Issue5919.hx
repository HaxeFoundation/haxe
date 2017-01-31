package unit.issues;

class Issue5919 extends unit.Test {
#if php
	function test() {
		untyped __php__("$_TEST_VAR = ['test' => 12]");
		t(untyped __call__("is_array", untyped __var__("_TEST_VAR")));
		eq(untyped __var__("_TEST_VAR", "test"), 12);
	}
#end
}