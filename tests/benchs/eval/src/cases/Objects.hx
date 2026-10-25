package cases;

/** Allocation of objects, field accesses, virtual and final method calls. **/
class Objects {
	public static function run() {
		var shapes:Array<Shape> = [];
		for (i in 0...1000)
			shapes.push(i % 3 == 0 ? new Shape(i, i + 1) : i % 3 == 1 ? new Square(i) : new Rect(i, 2));
		var sum = 0;
		for (k in 0...100) {
			for (s in shapes) {
				sum = (sum + s.area() + s.perimeter()) & 0xFFFFFF;
				s.move(1, k);
			}
			var p = new Point(k, k);
			for (i in 0...200)
				p = p.add(new Point(i, 1));
			sum = (sum + p.x + p.y) & 0xFFFFFF;
		}
		return sum;
	}
}

private class Point {
	public final x:Int;
	public final y:Int;

	public function new(x, y) {
		this.x = x;
		this.y = y;
	}

	public function add(p:Point) {
		return new Point(x + p.x, y + p.y);
	}
}

private class Shape {
	public var x:Int;
	public var y:Int;

	public function new(x, y) {
		this.x = x;
		this.y = y;
	}

	public function area() {
		return 0;
	}

	public function perimeter() {
		return 0;
	}

	public function move(dx:Int, dy:Int) {
		x += dx;
		y = (y + dy) & 0xFFFF;
	}
}

private class Rect extends Shape {
	var w:Int;
	var h:Int;

	public function new(w, h) {
		super(0, 0);
		this.w = w;
		this.h = h;
	}

	override function area() {
		return w * h;
	}

	override function perimeter() {
		return 2 * (w + h);
	}
}

private class Square extends Rect {
	public function new(s) {
		super(s, s);
	}

	override function perimeter() {
		return 4 * w;
	}
}
