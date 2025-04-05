package unit.issues;

class Issue9281 extends unit.Test {
	@:analyzer(no_user_var_fusion)
	function test() {
		var expected = [1,2];
		function f() {
			var expected = expected;
			return expected;
		}
		eq(expected, f());
	}
}