package unit.issues;

class Issue5249 extends unit.Test {
	static function value() return 123;
	static function that() return 321;

	function test() {
		unit.issues.misc.Issue5429Macro.mb();
		@:mergeBlock {
			var a = value();
			var b = that();
		}
		eq(444, a + b); // not in scope
	}
}