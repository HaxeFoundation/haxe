package unit.issues;

class Issue4323 extends Test {

	function test() {
		setB();
		t(b);
	}

	function setB() {
		return doSetB();
	}

	static var b = false;

	static inline function doSetB() {
		b = true;
	}

}