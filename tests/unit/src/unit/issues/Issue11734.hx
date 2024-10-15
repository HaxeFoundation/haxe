package unit.issues;

import unit.Test;
#if hl
import hl.NativeArray;
#end

private class Group<T> {
	public var grid : haxe.ds.Vector<Array<T>>;
	public function new(size:Int) {
		grid = new haxe.ds.Vector(size);
		for (i in 0...size)
			grid[i] = [];
	}
}

private class Foo {
	public var x : Int;
	public function new(x:Int) {
		this.x = x;
	}
}

class Issue11734 extends Test {
	#if hl
	function test() {
		var a = new hl.NativeArray<Float>(1);
		a[0] = 0.0;
		var b:NativeArray<Float> = new hl.NativeArray<Float>(1);
		b[0] = 1.0;
		a.blit(0, b, 0, 1);
		feq(1.0, a[0]);
	}
	#end

	function testArrayInVector() {
		var g = new Group<Foo>(5);
		for (i in 0...5)
			g.grid[i].push(new Foo(10+i));
		eq(10, g.grid[0][0].x);
		eq(14, g.grid[4][0].x);

		var g = new Group<Float>(5);
		for (i in 0...5)
			g.grid[i].push(10.0+i);
		feq(10.0, g.grid[0][0]);
		feq(14.0, g.grid[4][0]);
	}
}
