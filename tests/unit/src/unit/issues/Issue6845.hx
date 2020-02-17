package unit.issues;

class Issue6845 extends unit.Test {
	static var tmp:Any;
	function test() {
		tmp = new Sample<Float>();
		tmp = new Sample<Bool>();
		eq(2, Sample.n);
	}
}

private class Sample<T> {
	public static var n:Int =0;
	public function new() {
		n++;
	}
}