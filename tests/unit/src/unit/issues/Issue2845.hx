package unit.issues;
import unit.Test;

class Issue2845 extends Test {
	function test() {
		var c = 0;
		eq(4, (36 >>> c) & ((1 << 4) - 1));
	}
}