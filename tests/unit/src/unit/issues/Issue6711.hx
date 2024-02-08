package unit.issues;

private class V2 {
	public var x:Float;
	public var y:Float;

	inline public function new(x, y) {
		this.x = x;
		this.y = y;
	}
}

class Issue6711 extends unit.Test {
	function test() {
		eq("(0.0,1.0)(0.0,1.0)(0.0,1.0)", run());
	}

	inline static function getPos():V2 {
		return new V2(0, 1);
	}

	inline static public function iteration(func) {
		func();
		func();
		func();
	}

	public static function run() {
		var r = "";
		iteration(function() {
			var wpos = getPos();
			r += '(${wpos.x},${wpos.y})';
		});
		return r;
	}
}
