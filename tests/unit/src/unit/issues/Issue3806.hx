package unit.issues;

private abstract Pointer<T>(Int) from Int {
    @:op(A - B)
	public static function subP<T>(lhs:Pointer<T>, rhs:Pointer<T>):Pointer<T> {
		return lhs.value() - rhs.value();
	}

    public inline function value() {
        return this;
    }
}

class Issue3806 extends Test {
	function test() {
        var p:Pointer<Int> = 12;
        var p2:Pointer<Int> = 5;
		var i = (cast (p - p2).value());
		eq(7, i);
	}
}