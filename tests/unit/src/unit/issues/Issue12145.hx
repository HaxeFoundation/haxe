package unit.issues;

import utest.Assert;

@:structInit
final private class PointImpl {
	public var x:Float;
}

@:forward
private abstract Point(PointImpl) from PointImpl to PointImpl {
	@:op(a - b)
	public function sub_p(p:Point):Vec
		return {x: this.x - p.x};
}

@:structInit
final private class VecImpl {
	public var x:Float;
}

@:forward
private abstract Vec(VecImpl) from VecImpl to VecImpl {
	// comment out this function and compilation will succeed
	@:op(a - b)
	public function sub_v(v:Vec):Vec
		return {x: this.x - v.x};
}

class Issue12145 extends Test {
	function test() {
		final a:Point = {x: 2};
		final b:Point = {x: 1};
		// success if remove the :Vec, and even determines the correct type
		// however the same scenario can be repro'd by passing the result of the subtraction to a function which accepts Vec
		final c:Vec = a - b;
		feq(1.0, c.x);
	}
}
