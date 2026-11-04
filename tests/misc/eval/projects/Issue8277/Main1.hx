class Main {
	static final Up = new Point(0, -1);
	static final Down = new Point(0, 1);

	static function main() {}

	function opposite(direction:Point):Point {
		return switch (direction) {
			case Up: Down;
		}
	}
}

class Point {
	public final x:Int;
	public final y:Int;

	public function new(x, y) {
		this.x = x;
		this.y = y;
	}
}