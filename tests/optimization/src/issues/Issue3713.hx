package issues;

private class A<T> {
    public function new() {}
}

@:native("BImpl")
private abstract B<T>(A<T>) {
    public function new() this = new A();
    public inline function f():C<T> return new C(this);
}

private class C<T> {
	var x:Int;
    public inline function new(d:A<T>) {
		x = 1;
	}
}

class Issue3713 {
	@:js('
		var b = BImpl._new();
		var c_x = 1;
	')
	@:analyzer(no_const_propagation, no_local_dce, no_check_has_effect)
	static function test() {
        var b = new B<Int>();
        var c = b.f();
	}
}