package unit.issues;
import unit.Test;

private abstract A1(Int) to Int {
	public inline function new(i) {
		this = i;
	}

	function get() {
		return this;
	}

	@:commutative
	@:op(A + B)
	static function add(lhs:A1, rhs:ABase):A1;

	@:op(A - B)
	function sub(rhs:ABase):A1 {
		return new A1(this - rhs.get());
	}

	@:commutative @:op(A * B)
	static function mul(lhs:A1, rhs:ABase) {
		return lhs.get() * rhs.get();
	}

	@:op(A % B)
	function mod(rhs:ABase):A1;
}

private abstract ABase(Int) {
	public inline function new(i) {
		this = i;
	}

	public function get() {
		return this;
	}
}

private abstract AChild(Int) {
	public inline function new(i) {
		this = i;
	}

	@:to public function toABase() {
		return new ABase(this * 2);
	}
}

class Issue2661 extends Test {
	function test() {
		var a1 = new A1(3);
		var ac = new AChild(2);
		var i = 0;
		eq(2, new A1(i++) + new AChild(i++));
		eq(2, i);
		eq(7, new AChild(i++) + new A1(i++));
		eq(4, i);
		eq(-6, new A1(i++) - new AChild(i++));
		eq(6, i);
		eq(84, new A1(i++) * new AChild(i++));
		eq(8, i);
		eq(144, new AChild(i++) * new A1(i++));
		eq(10, i);
		eq(10, new A1(i++) % new AChild(i++));
		eq(12, i);

		t(unit.TestType.typeError(new AChild(1) - new A1(0)));
		t(unit.TestType.typeError(new AChild(1) % new A1(1)));
	}
}