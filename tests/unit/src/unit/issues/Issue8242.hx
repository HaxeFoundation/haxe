package unit.issues;

class Issue8242 extends unit.Test {
	@:analyzer(ignore)
	public function test() {
		var num:Null<Int> = 22;
		var i = (num - 1) * 2;
		eq(42, i);

		var num:Null<Float> = 22;
		var i = (num - 1) * 2;
		eq(42.0, i);
	}
}