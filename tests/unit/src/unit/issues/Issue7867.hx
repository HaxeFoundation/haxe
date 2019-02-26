package unit.issues;

#if js
private class A {
	@:native("##") public static function f() return 10;
}
#end

class Issue7867 extends unit.Test {
	#if js
	function test() {
		eq(Reflect.callMethod(A, Reflect.field(A, "##"), []), 10);
		eq(A.f(), 10);
	}
	#end
}
