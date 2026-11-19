package unit.issues;

class Issue13204 extends unit.Test {
	#if eval
	static var max = 2147483647;
	static var min = -2147483648;
	static var one = 1;
	static var minusOne = -1;

	function testWrap() {
		eq(min, max + one);
		eq(max, min - one);
		eq(1, max * max);
		eq(0, min * min);
		eq(min, -min);
		feq(-2147483648., Math.abs(min));
		eq(0, min % minusOne);

		var i = max;
		i++;
		eq(min, i);
		eq(max, --i);
	}

	function testShifts() {
		eq(min, one << 31);
		eq(1, one << 32);
		eq(-1, minusOne >>> 0);
		eq(max, minusOne >>> 1);
		eq(-1, min >> 31);
	}

	function testBitOps() {
		eq(-1, max ^ min);
		eq(0, max & min);
		eq(-1, max | min);
		eq(min, ~max);
	}

	function testDynamic() {
		var d:Dynamic = max;
		var sum:Dynamic = d + 1;
		var product:Dynamic = d * d;
		eq(min, sum);
		eq(1, product);
		t(Std.isOfType(sum, Int));
		eq("-2147483648", Std.string(sum));
	}
	#end
}
