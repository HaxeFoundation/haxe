package unit.issues;

final class Vec {
	public var x:Float;

	public inline function new(x:Float)
		this.x = x;
}

final class Rect {
	public var top_left:Vec;

	public inline function new(top_left:Vec)
		this.top_left = top_left;
}

enum Shape {
	Vec(v:Vec);
}

interface BodyInt {
	function shape():Shape;
}


class Body implements BodyInt {
	public inline function shape():Shape {
		throw new Rect(new Vec(1)).top_left;
	}
}

class Issue12149 extends Test {
	function test() {
		noAssert();
	}

	static inline function update_entity<T:BodyInt>(body:T) {
		switch body.shape() {
			case Vec(v):
				throw new Vec(new Vec(new Vec(v.x).x).x);
			default:
				throw "";
		}
	}
	
	static function set_pos(body:Body)
		update_entity(body);
}