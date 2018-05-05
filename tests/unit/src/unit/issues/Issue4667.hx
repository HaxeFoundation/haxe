package unit.issues;

class Issue4667 extends Test {
	@:analyzer(no_fusion)
	function test() {
		var s = String;
		eq("A", s.fromCharCode(65));
	}
}