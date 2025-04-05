package unit.issues;

private abstract Abstracted<A>(Base<A>) {
	public inline function new(a) this = new Base(a);

	public function getThis() {
		return this;
	}
}

private class Base<A> {
	public var a:A;
	public function new(a) this.a = a;
}

class Issue9764 extends unit.Test {
	function test() {
		var fnew = Abstracted.new;
		var a = fnew(1);
		eq(1, a.getThis().a);
	}
}