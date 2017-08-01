package unit.issues;

class Issue6482 extends unit.Test {
	function test() {
		#if (!cpp && !cs && !java && !lua)
		exc(function() {
			cast("foo", Int);
			trace("fail");
		});
		#end
	}
}