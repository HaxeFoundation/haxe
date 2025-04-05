package unit.issues;

class Issue9813 extends unit.Test {
	var my : Int;
	var cb : Int->Void;

	function test() {
		cb = n -> switch n {
			case 1: my = 2;
			case _: my = 3;
		};
		cb(12);
		eq(3, my);
	}
}