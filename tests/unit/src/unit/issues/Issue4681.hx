package unit.issues;

private abstract A(Int) {
	public function new(i) {
		this = i;
	}

	public function get() {
		return this;
	}

	@:op(A + B)
	static function add(x:A, x2:Int) {
		return new A(x.get() + x2);
	}

	@:op(A * B)
	@:commutative
	static function multiply(x:A, x2:Int) {
		return x.get() * x2;
	}
}

class Issue4681 extends unit.Test {

	var array:Array<A>;
	var field:A;
	var array2:Array<Int>;
	var field2:Int;

	function getArray() {
		return array;
	}

	function getArray2() {
		return array2;
	}

	function getThis(i:Int) {
		return this;
	}

	function getA() {
		return new A(0);
	}

	function setup() {
		array = [new A(0)];
		field = new A(0);
		array2 = [2];
		field2 = 2;
	}

	public function testNormal() {
		setup();
		var a = new A(0);
		a += 1;
		eq(1, a.get());
	}

	public function testNormal2() {
		setup();
		var a = 2;
		a *= new A(3);
		eq(6, a);
	}

	public function testArray() {
		setup();
		var x = 0;
		getArray()[x++] += 1;
		eq(1, array[0].get());
		eq(1, x);
	}

	public function testArray2() {
		setup();
		var x = 0;
		getArray2()[x++] *= new A(3);
		eq(6, array2[0]);
		eq(1, x);
	}

	public function testField() {
		setup();
		var x = 0;
		getThis(x++).field += 1;
		eq(1, x);
		eq(1, field.get());
	}

	public function testField2() {
		setup();
		var x = 0;
		getThis(x++).field2 *= new A(3);
		eq(1, x);
		eq(6, field2);
	}

	public function testNested() {
		setup();
		var x = 0;
		getArray()[(getArray()[x++] += 1).get() - 1] += 1;
		eq(2, array[0].get());
		eq(1, x);
	}

	public function testNested2() {
		setup();
		var x = 0;
		getArray2()[(getArray2()[x++] *= new A(3)) - 6] *= new A(3);
		eq(18, array2[0]);
		eq(1, x);
	}
}