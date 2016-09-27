package unit.issues;
import unit.Test;

class Issue3191 extends Test {

	function test() {
		var it = [1, 2, 3].iterator();
		eq(1, it.next());
		eq(true, it.hasNext());
		eq(2, it.next());
		eq(true, it.hasNext());
		eq(3, it.next());
		eq(false, it.hasNext());
	}
}