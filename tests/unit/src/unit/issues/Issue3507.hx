package unit.issues;

private class A<T1> {
	public var t1:T1;
	public function new(t1:T1) {
		this.t1 = t1;
	}
}

private class B<T1, T2> extends A<T1> {
	public var t2:T2;
	public function new(t1:T1, t2:T2) {
		super(t1);
		this.t2 = t2;
	}
}

private class C<T1, T2, T3> extends B<T1, T2> {
	public var t3:T3;
	public function new(t1:T1, t2:T2, t3:T3) {
		super(t1, t2);
		this.t3 = t3;
	}
}

class Issue3507 extends Test {
	function test() {
		var c = new C("foo", 12, false);
		var s = switch (c) {
			case { t1:'foo', t2:12, t3:false } : "ok";
			case _: "not ok";
		}
		eq("ok", s);
	}
}