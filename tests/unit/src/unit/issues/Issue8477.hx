package unit.issues;

class Issue8477 extends unit.Test {
	function test() {
		var f:Float = 3;
		var d:Dynamic = f;
		t(Type.typeof(d).match(TInt));
		f /= 2;
		d = f;
		t(Type.typeof(d).match(TFloat));
	}
}
