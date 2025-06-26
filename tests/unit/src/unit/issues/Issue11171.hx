package unit.issues;

inline function getBlendFunc(value:Int):Context3DBlendFactor {
	switch (value) {
		case 0:
			return Context3DBlendFactor.ZERO;
		case 1:
			return Context3DBlendFactor.ONE;
		default:
			throw "unsupported blending function";
	}
}

class Issue11171 extends Test {
	function test() {
		getBlendFunc(0);
		utest.Assert.pass();
	}
}

private enum abstract Context3DBlendFactor(Int) {
	public var ONE = 2;
	public var ZERO = 9;
}
