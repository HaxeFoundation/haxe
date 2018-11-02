package unit.issues;

class Issue7533 extends unit.Test {
	function test() {
      var bint:Int = 0xB425B745;
      eq( (bint>>>20), 0xB42);
	}
}