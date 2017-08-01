package unit.issues;

class Issue6482 extends unit.Test {
	function test() {
		exc(function() {
			cast("foo", Int);
			trace("fail");
		});
	}
}