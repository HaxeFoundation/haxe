package unit.issues;

class Issue8930 extends Test {
	var v:Bar = {x:10};

	function test() {
		eq(11, ++v.x);
		eq(11, v.x);
		eq(11, v.x++);
		eq(12, v.x);

		eq(11, --v.x);
		eq(11, v.x);
		eq(11, v.x--);
		eq(10, v.x);

		#if !as3
		var cnt = 0;
		function sideEffect() {
			cnt++;
			return v;
		}
		++sideEffect().x;
		sideEffect().x++;
		eq(2, cnt);
		#end
	}
}

private typedef Foo = {
	x: Int
}

private abstract Bar(Foo) from Foo to Foo {
	public var x(get, set):Int;
	public inline function get_x() return this.x;
	public inline function set_x(value) return this.x = value;
}
