package unit.issues;

class IssueDynNullEq extends unit.Test {
	function test() {
		var d:Dynamic = null;
		f(d == 0);
		t(d != 0);
		f(d == 0.0);
		var i:Dynamic = 0;
		t(i == 0);
		t(i == 0.0);
		var j:Dynamic = 1;
		t(j == 1);
		f(j == 2);
	}
}
