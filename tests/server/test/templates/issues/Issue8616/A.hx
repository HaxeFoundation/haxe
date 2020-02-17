abstract A(C) {
	public inline function f(v:Int)
		this.f(v);
}

class C {
	public inline function f(v:Int) {
		use(v);
		use(v);
	}

	function use(v:Int) {}
}
