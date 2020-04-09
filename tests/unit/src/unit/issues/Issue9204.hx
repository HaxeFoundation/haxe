package unit.issues;

class Issue9204 extends unit.Test {
	@:deprecated
	static inline final DEPRECATED = 123;

	function test() {
		eq(1, switchWithDeprecated(123));
		eq(2, switchWithDeprecated(321));
	}

	static public function switchWithDeprecated(x:Int) {
		return switch x {
			case DEPRECATED: 1;
			case _: 2;
		}
	}
}