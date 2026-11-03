package unit.issues;

private abstract Vector(Array<Float>) {
	public function new(a:Array<Float>)
		this = a;

	@:op([])
	public function get(index:Int):Float;

	@:op([])
	public function set(index:Int, value:Float):Float;
}

class Issue12989 extends unit.Test {
	function test() {
		var co = new Vector([10, 1]);
		co[0] += 10;
		eq(20., co[0]);
		co[1] *= 3;
		eq(3., co[1]);
		var i = 0;
		co[i++] -= 5;
		eq(1, i);
		eq(15., co[0]);
		eq(18., co[1] += co[0]);
	}
}
