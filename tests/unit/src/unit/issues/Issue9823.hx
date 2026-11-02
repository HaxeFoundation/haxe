package unit.issues;

private abstract TestAbstract<T>(T) {
	public var access(get, set):T;

	inline function get_access()
		return this;

	inline function set_access(value:T)
		return this = value;

	public inline function new(value:T)
		this = value;
}

private abstract TestAbstractOverAbstract(TestAbstract<Float>) {
	public var value(get, never):Float;

	inline function get_value()
		return this.access;

	public inline function new(value:Float)
		this = new TestAbstract(value);

	@:op(A += B) inline function addAssign(val:Float)
		return new TestAbstractOverAbstract(this.access += val);

	@:op(A -= B) inline function subAssign(val:Float)
		return new TestAbstractOverAbstract(this.access -= val);
}

class Issue9823 extends Test {
	function test() {
		var a = new TestAbstractOverAbstract(1.5);
		a += 1.25;
		feq(2.75, a.value);
		a -= 0.5;
		feq(2.25, a.value);
	}
}
