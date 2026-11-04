package unit.issues;

@:generic
private class Vec<T, @:const L:Int> {
	public var data:Array<T>;

	public function new(...values:T) {
		if (values.length > L)
			throw "Too many values for Vec of length " + L;
		data = [];
		for (i in 0...L)
			data.push(values[i]);
	}
}

class Issue12652 extends Test {
	function test() {
		var vec:Vec<Float, 4> = new Vec(5, 4, 8, 10);
		eq("5,4,8,10", vec.data.join(","));

		var vec:Vec<Int, 2> = new Vec(1, 2);
		eq(2, vec.data.length);
		eq(3, vec.data[0] + vec.data[1]);
	}
}
