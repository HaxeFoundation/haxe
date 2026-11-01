package unit.issues;

using Issue12380.ArrayExtensions;
private class ArrayExtensions {
	public static inline function isEmpty(a : Array<Any>) : Bool {
		return a.length == 0;
	}
}

@:forward(push)
private abstract IPolygon(Array<IPoint>) from Array<IPoint> to Array<IPoint> {
}

private class IPoint {
	public var x : Int;
	public var y : Int;
	public inline function new(x = 0, y = 0) {
		this.x = x;
		this.y = y;
	}
}

class Issue12380 extends Test {
	function test() {
		var shape : IPolygon = [];
		shape.push(new IPoint(0, 1));
		f(shape.isEmpty());
	}
}
