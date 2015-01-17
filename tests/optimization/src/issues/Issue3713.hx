package issues;

private class A<T> {
    public function new() {}
}

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
		var b = issues._Issue3713.B_Impl_._new();
		var c_x = 1;
	')
	@:analyzer(no_const_propagation, no_local_dce, no_check_has_effect)
	static function test() {
        var b = new B<Int>();
        var c = b.f();
	}
}