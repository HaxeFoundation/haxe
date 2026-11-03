package unit.issues;

@:structInit
private class Foo {
	public final points:Array<Point> = [for (i in 0...10) {y: i}];
}

@:structInit
private class Point {
	public final x = 42;
	public final y:Int;
}

class Issue9289 extends Test {
	function test() {
		final foo:Foo = {points: [{y: 1}]};
		eq(1, foo.points.length);
		eq(42, foo.points[0].x);
		eq(1, foo.points[0].y);

		final foo2:Foo = {};
		eq(10, foo2.points.length);
		eq(42, foo2.points[3].x);
		eq(3, foo2.points[3].y);
	}
}
