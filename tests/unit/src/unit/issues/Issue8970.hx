package unit.issues;

class Issue8970 extends unit.Test {
	function test() {
		eq(-16, Std.parseInt('	  -0x10'));
	}
}