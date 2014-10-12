package unit.issues;

private class AA
{
    public var x:Float;
    public var y:Float;

    inline public function new(x:Float, y:Float)
    {
        this.x = x;
        this.y = y;
    }
}

private abstract BB(AA)
{
    public var x(get, set):Float;
    public var y(get, set):Float;


    inline public function get_x():Float return this.x;
    inline public function get_y():Float return this.y;

    inline public function set_x(v:Float):Float return this.x = v;
    inline public function set_y(v:Float):Float return this.y = v;

    public inline function new(x:Float = 0, y:Float = 0)
    {
        this = new AA(x, y);
    }

    @:commutative @:op(A*B) inline public static function opMulSF(a:BB, s:Float):BB
    {
        return new BB(a.x * s, a.y * s);
    }
}


class Issue2396 extends Test {
	function test() {
        var a = new BB(10, 20);

        var t1 = 1.0;
        var t2 = 2.0;

        // !!! fails here
        var fail = a * (t1 + t2);
		feq(30, fail.x);
		feq(60, fail.y);

        // ok here
        var cacheSum = (t1 + t2);
        var ok = a * cacheSum;
		feq(30, ok.x);
		feq(60, ok.y);
	}
}