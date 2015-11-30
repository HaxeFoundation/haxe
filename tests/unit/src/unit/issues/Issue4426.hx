package unit.issues;

private class Vector2Base {
	public var x:Float;
	public var y:Float;

	public inline function new(x:Float, y:Float) {
		this.x = x;
		this.y = y;
	}

	public function toString() {
		return '($x, $y)';
	}
}

@:forward(x, y)
private abstract Vector2(Vector2Base) from Vector2Base to Vector2Base {

	public inline function new(x:Float = 0.0, y:Float = 0.0) {
		this = new Vector2Base(x, y);
	}

	@:op(A + B)
	public static inline function Add(a:Vector2, b:Vector2):Vector2
	{
		return new Vector2(a.x + b.x, a.y + b.y);
	}
}

private class Transform {
	public var position(default, set):Vector2 = new Vector2(0.0, 0.0);

	public function new() {
	}

   	function set_position(newPosition:Vector2):Vector2 {
		position = newPosition;
		return position;
	}
}

class Issue4426 extends Test {
	function test() {
		var transform = new Transform();
		transform.position += new Vector2(1.0, 0.0);
		feq(1., transform.position.x);
		feq(0., transform.position.y);
	}
}