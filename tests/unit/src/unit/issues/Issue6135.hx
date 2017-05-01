package unit.issues;

class Issue6135 extends unit.Test {
	@:analyzer(ignore)
	function test() {
		var x:Array<Dynamic> = [1];
		x[0] = "hi";
		t(x[0] == "hi");
	}
}