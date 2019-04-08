package unit.issues;

class Issue7449 extends unit.Test {
	#if !(neko || (cpp && !cppia && !hxcpp_smart_strings))
	@Ignored
	function test() {
		eq(220, /*"\xDC".charCodeAt(0)*/220);
	}
	#end
}