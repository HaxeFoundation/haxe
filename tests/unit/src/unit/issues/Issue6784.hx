package unit.issues;

class Issue6784 extends Test {
	function test() {
		#if cs
		cs.system.Console.BufferHeight;
		#end
		noAssert();
	}
}
