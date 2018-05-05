package unit.issues;

import unit.Test;

private abstract O(Int) {
	public inline function get():Int return this;
}

class Issue4751 extends Test{
	static function func(v:Float) {}

	function testNadako() {
		var o:{?v:O} = {};
		func(if (cond()) o.v.get() else throw "oh");
	}

	function testGama() {
		var dx:Float = Math.random();
		var dy:Float = Math.random();
		{
			var t = dx;
			dx = -dy;
			dy = t;
		};
		func(dy * 1.1);
	}

	static function cond() return true;
}