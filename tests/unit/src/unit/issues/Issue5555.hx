package unit.issues;

class Issue5555 extends unit.Test {
	static var flag = false;

	function testStateWrite() {
		var call1 = pureCall();
		var call2 = impureCall();
		f(call1 && call2);
		t(flag);
	}

	function testVarWrite() {
		var x = getInt();
		var call1 = pureCall();
		var y = x++;
		f(call1 && (y == 0));
		eq(1, x);
	}

	function testFieldWrite() {
		var o = getObject();
		var call1 = pureCall();
		var y = o.value = (getInt() + 1);
		f(call1 && (y == 0));
		eq(1, o.value);
	}

	static function pureCall() {
		return false;
	}

	@:pure(false)
	static function impureCall() {
		flag = true;
		return false;
	}

	static function getInt() {
		return 0;
	}

	static function getObject() {
		return { value: 0 };
	}
}