package unit.issues;

class Issue6548 extends unit.Test {

	static function obj(_) return { pos: 12 };

	function test() {
		switch 1 {
			case _ * 2 => 2:
			case _ * 2 => 3:
			case obj(_) => { pos: pos }:
			case _:
		}
	}
}