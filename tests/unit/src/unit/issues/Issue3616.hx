package unit.issues;

private abstract A(Int) {
    public static function f(v:Int):Void {}
    public static function z():Void {}
}

private abstract B<T>(T) {
    public static function f<T>(v:T):B<T> return cast v;
}

class Issue3616 extends Test {
	function test() {
        var v:A = null;
		eq(unit.TestType.typeErrorText(v.f(1)), "Invalid call to static function f through abstract instance");

		var v = B.f(10);
		eq(unit.TestType.typeErrorText(v.f()), "Invalid call to static function f through abstract instance");
	}
}
