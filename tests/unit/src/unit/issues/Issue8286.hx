package unit.issues;

class Issue8286 extends unit.Test {
	function test() {
		eq(null, Type.getClass({a: 1}));
	}
}