package unit.issues;

class Issue7179 extends unit.Test {
	function test() {
		var f:(?f:Float) -> Void = function(?_) { }
		f();
		f(1.);
	}
}