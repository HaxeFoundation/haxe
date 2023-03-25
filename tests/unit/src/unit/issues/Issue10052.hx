package unit.issues;

class Issue10052 extends Test {
	function test() {
		final player = new Player();
		player.position += new Point(3, 4);
		eq(4, player.position.x);
		eq(6, player.position.y);
	}
}

@:forward(x, y)
private abstract Point({x:Int, y:Int}) {
	public inline function new(x:Int, y:Int) {
		this = {x: x, y: y};
	}

	@:op(A + B) inline function add(other:Point):Point {
		return new Point(this.x + 2 * other.x, this.y + 2 * other.y);
	}

	@:op(A += B) public inline function set_add(other:Point):Void {
		this.x += other.x;
		this.y += other.y;
	}
}

private class Player {
	public final position = new Point(1, 2);
	public function new() {}
}