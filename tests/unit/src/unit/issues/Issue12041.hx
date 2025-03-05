package unit.issues;

class Issue12041 extends Test {
	#if (java || hl || cpp)
	@:analyzer(ignore)
	function testCastNullFloat() {
		var fnull : Null<Float> = null;
		var snull : Null<Single> = null;
		var f0 : Float = snull;
		feq(f0, 0);
		var s0 : Single = fnull;
		feq(s0, 0);
	}

	@:analyzer(ignore)
	function testSingleOp() {
		var s1 : Single = 10.0;
		feq(10.0, s1);
		var s2 : Single = 0.3;
		feq(0.3, s2);
		var f1 : Float = 10.0;
		var f2 : Float = 0.3;
		var a : Single = s1 + s2;
		feq(10.3, a);
		var a : Single = s1 - s2;
		feq(9.7, a);
		var a : Single = s1 * s2;
		feq(3.0, a);
		var a : Single = s1 / s2;
		feq(33.3333333333, a);
		var a : Single = s1 / (f2 : Single);
		feq(33.3333333333, a);
		var a : Single = (f1 : Single) / s2;
		feq(33.3333333333, a);
		var a : Single = (f1 : Single) / (f2 : Single);
		feq(33.3333333333, a);
	}

	@:analyzer(ignore)
	function testSingleFromInt() {
		var s1 : Single = 10;
		feq(10.0, s1);
		var s2 : Single = 3;
		feq(3.0, s2);
	}
	#end
}
