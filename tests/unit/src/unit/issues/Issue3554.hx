package unit.issues;

private abstract A(Int)
{
    public static var a(get,never):Int;
    private static function get_a()
        return(1 << x);

    public static var x(get,never):Int;
    static var _x:Int = 4;
    inline static function get_x()
        return _x;
}


class Issue3554 extends Test {
	function test() {
		eq(16, A.a);
	}
}