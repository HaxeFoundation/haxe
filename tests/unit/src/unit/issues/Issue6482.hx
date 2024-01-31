package unit.issues;

class Issue6482 extends unit.Test {
	#if (!cpp && !java && !lua)
	function test() {
		exc(function() {
			cast("foo", Int);
			trace("fail");
		});
	}
	#end
}
