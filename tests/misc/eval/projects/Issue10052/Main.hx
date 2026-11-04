@:forward(x, y)
abstract Point({x:Float, y:Float}) {
	public inline function new(x:Float, y:Float) {
		this = {x: x, y: y};
	}

	@:op(A + B) inline function add(other:Point):Point {
		return new Point(this.x + 2 * other.x, this.y + 2 * other.y);
	}
}

class Player {
	public final position = new Point(0, 0);
	public function new() {}
}

class Main {
	static function main() {
		final player = new Player();
		player.position += new Point(1, 1);
	}
}