package unit.issues;

class Issue4695 extends unit.Test {

	function eqCheck<T>(v1:T, v2:T) {
		return v1 == v2;
	}
	
	public function testNull() {
		f("" == null);
		f(eqCheck("", null));
		#if !(cpp || flash9 || as3 || java || cs)
		f(false == null);
		f(eqCheck(false, null));
		#end
	}
	
	#if php
	public function testNativeArray() {
		var a1 = (untyped __php__)('array("f1" => 1, "f2" => 2, "f3" => 3)');
		var a2 = (untyped __php__)('array("f1" => true, "f2" => "2", "f3" => 3)');
		f(a1 == a2);
		f(eqCheck(a1, a2));
	}
	#end
	
}
