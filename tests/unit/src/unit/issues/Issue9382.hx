package unit.issues;

class Issue9382 extends unit.Test {

	function test() {
		var buf = new StringBuf();
		buf.addSub('🦖', 0);
		eq('🦖', buf.toString());
	}
}