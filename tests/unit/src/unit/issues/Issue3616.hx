package unit.issues;

private abstract A(Null<Int>) {
	public static function f(v:Int):Void {}
	public static function z():Void {}
}

private abstract B<T>(T) {
	public static function f<T>(v:T):B<T> return cast v;
}

private abstract C(Null<Int>) {
	public static var f(get,set):Int;
	static function get_f() return 1;
	static function set_f(value) return 1;
}

class Issue3616 extends Test {
	function test() {
		var v:A = null;
		eq(unit.HelperMacros.typeErrorText(v.f(1)), "Invalid call to static function f through abstract instance");

		var v = B.f(10);
		eq(unit.HelperMacros.typeErrorText(v.f()), "Invalid call to static function f through abstract instance");

		var v:C = null;
		eq(unit.HelperMacros.typeErrorText(v.f), "Invalid call to static function f through abstract instance");
		eq(unit.HelperMacros.typeErrorText(v.f = 5), "Invalid call to static function f through abstract instance");
	}
}
