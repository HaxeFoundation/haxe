package unit.issues;

class Issue5859 extends unit.Test {
	@:analyzer(no_const_propagation)
	@:analyzer(no_fusion)
	function test() {
		var await = 1;
		eq(1, await);
	}
}