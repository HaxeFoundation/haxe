package unit.issues;

class Issue7449 extends unit.Test {
	/**
	 * Not happens since https://github.com/HaxeFoundation/haxe/pull/8141
	 */
	// #if !(neko || (cpp && !cppia && !hxcpp_smart_strings))
	// function test() {
	// 	eq(220, "\xDC".charCodeAt(0));
	// }
	// #end
}