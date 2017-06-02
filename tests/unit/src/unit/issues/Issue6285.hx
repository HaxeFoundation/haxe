package unit.issues;

private enum E1 {
	A1;
	B1;
	C1;
}

private enum E2 {
	A2;
	B2;
	C2;
}

class Issue6285 extends unit.Test {
	function test() {
		f(getA1() == getA2());
	}

	static function getA1():Dynamic {
		return A1;
	}

	static function getA2():Dynamic {
		return A2;
	}
}