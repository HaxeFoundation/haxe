package unit.issues;

class Issue7179 extends unit.Test {
	function test() {
		var f:(?f:Null<Float>) -> Void = function(?_) { }
		f();
		f(1.);
		noAssert();
	}
}