package unit;

import haxe.ds.HashMap;

class TestHashMap extends Test {
	function test() {
		var grid = new HashMap<Point, String>();
		grid[new Point(0, 0)] = "a";
		grid[new Point(0, 1)] = "b";
		grid[new Point(1, 0)] = "c";
		grid[new Point(1, 1)] = "d";

		eq("c", grid[new Point(1, 0)]);

		var asserts = 0;
		for (p => s in grid) {
			t(p.equals(switch s {
				case "a": new Point(0, 0);
				case "b": new Point(0, 1);
				case "c": new Point(1, 0);
				case "d": new Point(1, 1);
				case v: throw 'unknown value $v';
			}));
			asserts++;
		}
		eq(4, asserts);
	}
}

private class Point {
	public final x:Int;
	public final y:Int;

	public function new(x, y) {
		this.x = x;
		this.y = y;
	}

	public function equals(point:Point):Bool {
		return x == point.x && y == point.y;
	}

	public function hashCode():Int {
		return x + 10000 * y;
	}
}
