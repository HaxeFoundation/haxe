package unit.issues;

class Issue5358 extends unit.Test {

	#if false
	static var example = if (veryRandom() > 0.5) 1 else 2;

	function test() {
		eq(example, 1);
	}

	static function veryRandom() {
		return 4;
	}
	#end
}