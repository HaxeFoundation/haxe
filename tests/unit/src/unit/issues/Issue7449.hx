package unit.issues;

class Issue7449 extends unit.Test {
	function test() {
		#if !(neko || (cpp && !cppia && !hxcpp_smart_strings))
		eq(220, "\xDC".charCodeAt(0));
		#end
	}
}