package unit.teststd.haxe;

import haxe.UInt32;

class TestUInt32 extends unit.Test {
	static final ZERO:UInt32 = UInt32.MIN;
	static final ONE:UInt32 = 1;
	static final MAX:UInt32 = UInt32.MAX;
	// High-bit set: looks negative as signed Int
	static final HIGH:UInt32 = cast 0x80000000;

	// --- Constants ---
	function testMinMax() {
		// MIN is 0
		eq(ZERO.toInt(), 0);
		eq(Std.string(ZERO), "0");

		// MAX is 4294967295 (2^32 - 1)
		eq(Std.string(MAX), "4294967295");
	}

	// --- toString ---
	function testToString() {
		eq(Std.string(UInt32.fromInt(0)), "0");
		eq(Std.string(UInt32.fromInt(1)), "1");
		eq(Std.string(UInt32.fromInt(100)), "100");
		eq(Std.string(UInt32.fromInt(2147483647)), "2147483647"); // Int.MAX
		// 2^31 (would be MIN_INT32 in signed)
		eq(Std.string(HIGH), "2147483648");
		// MAX
		eq(Std.string(MAX), "4294967295");
	}

	// --- Unsigned comparison: key semantic difference from Int32 ---
	function testCompare() {
		// 0 < 1: trivial
		t(ZERO < ONE);
		t(ONE > ZERO);

		// 0x80000000 (HIGH) > 0x7FFFFFFF (Int.MAX): high bit means BIGGER in unsigned
		// But in signed Int, 0x80000000 is negative (less than 0x7FFFFFFF).
		t(HIGH > cast(0x7FFFFFFF, UInt32));
		t(HIGH > ONE);
		f(HIGH < ONE);

		// MAX > HIGH
		t(MAX > HIGH);
		t(MAX > ONE);
		f(MAX < ONE);

		// MAX == MAX
		t(MAX == MAX);
		f(MAX != MAX);
		t(MAX >= MAX);
		t(MAX <= MAX);

		// compare() function: same contract as Int64.compare
		t(UInt32.compare(ZERO, ONE) < 0);
		t(UInt32.compare(ONE, ZERO) > 0);
		eq(UInt32.compare(ONE, ONE), 0);
		// Unsigned ordering: HIGH > 0x7FFFFFFF
		t(UInt32.compare(HIGH, cast(0x7FFFFFFF, UInt32)) > 0);
		// MAX is the largest
		t(UInt32.compare(MAX, HIGH) > 0);
	}

	// --- Wrap-around arithmetic ---
	function testAddOverflow() {
		// MAX + 1 wraps to 0
		var r = MAX + ONE;
		eq(r.toInt(), 0);
		eq(r.toString(), "0");
	}

	function testSubUnderflow() {
		// 0 - 1 wraps to MAX
		var r = ZERO - ONE;
		eq(r.toString(), "4294967295");
		t(r == MAX);
	}

	function testMulWrap() {
		// MAX * 2 wraps: 4294967295 * 2 mod 2^32 = 4294967294
		var r = MAX * cast(2, UInt32);
		eq(r.toString(), "4294967294");
	}

	// --- Division: unsigned semantics ---
	function testDiv() {
		// Simple unsigned division
		var ten:UInt32 = 10;
		var three:UInt32 = 3;
		eq(Std.string(ten / three), "3");
		eq(Std.string(ten % three), "1");

		// Key: HIGH / 2  (would be negative if signed, but is 2^30 unsigned)
		// 0x80000000 / 2 = 0x40000000 = 1073741824
		var half = HIGH / cast(2, UInt32);
		eq(Std.string(half), "1073741824");

		// MAX / 2 = 2147483647 (floor)
		var maxHalf = MAX / cast(2, UInt32);
		eq(Std.string(maxHalf), "2147483647");
		var maxMod = MAX % cast(2, UInt32);
		eq(Std.string(maxMod), "1");
	}

	function testDivByZero() {
		var threw = false;
		try {
			var _ = ONE / ZERO;
		} catch (e:Dynamic) {
			threw = true;
		}
		t(threw);
	}

	// --- Shift: >> is always logical (unsigned) ---
	function testShr() {
		// Logical right shift: high bit does NOT get sign-extended
		var r = HIGH >> 1;
		eq(Std.string(r), "1073741824"); // 0x40000000

		// All bits set >> 4: zero-extends (logical)
		var allBits:UInt32 = cast 0xFFFFFFFF;
		var shifted = allBits >> 4;
		eq(Std.string(shifted), "268435455"); // 0x0FFFFFFF

		// >>> is the same as >> for unsigned
		eq(Std.string(allBits >>> 4), "268435455");
	}

	function testShl() {
		// Normal left shift wraps
		var r = ONE << 31;
		eq(r.toString(), "2147483648"); // 0x80000000 = HIGH
		t(r == HIGH);
	}

	// --- Bitwise ops ---
	function testBitwise() {
		var a:UInt32 = cast 0xF0F0F0F0;
		var b:UInt32 = cast 0x0F0F0F0F;
		eq(Std.string(a & b), "0");
		eq(Std.string(a | b), "4294967295"); // 0xFFFFFFFF
		eq(Std.string(a ^ b), "4294967295");
		eq(Std.string(~a), "252645135"); // 0x0F0F0F0F
		t((~a) == b);
	}

	// --- toFloat ---
	function testToFloat() {
		feq(ZERO.toFloat(), 0.0);
		feq(ONE.toFloat(), 1.0);
		// HIGH = 2^31 = 2147483648.0 (looks negative as signed, but correct as unsigned float)
		feq(HIGH.toFloat(), 2147483648.0);
		feq(MAX.toFloat(), 4294967295.0);
	}

	function testFloatComparisons() {
		#if loose_numeric_casts
		var five:UInt32 = 5;
		var fiveF:Float = 5.0;
		var threeF:Float = 3.0;
		var tenF:Float = 10.0;
		// UInt32 < Float
		t(five > threeF);
		f(five > tenF);
		t(five >= fiveF);
		f(five >= tenF);
		t(five < tenF);
		f(five < threeF);
		t(five <= fiveF);
		f(five <= threeF);
		// Float < UInt32
		t(threeF < five);
		f(tenF < five);
		t(fiveF <= five);
		f(tenF <= five);
		t(tenF > five);
		f(threeF > five);
		t(fiveF >= five);
		f(threeF >= five);
		#else
		noAssert();
		#end
	}

	// --- isZero ---
	function testIsZero() {
		t(UInt32.isZero(ZERO));
		f(UInt32.isZero(ONE));
		f(UInt32.isZero(MAX));
		f(UInt32.isZero(HIGH));
	}

	// --- Increment/Decrement ---
	function testIncDec() {
		var a:UInt32 = MAX;
		var prev = a++;
		t(prev == MAX);
		t(a == ZERO); // wraps

		var b:UInt32 = ZERO;
		var prev2 = b--;
		t(prev2 == ZERO);
		t(b == MAX); // underflows

		var c:UInt32 = ONE;
		t(++c == cast(2, UInt32));
		t(--c == ONE);
	}

	// --- UInt32 from/to Int mix ---
	function testIntMixedOps() {
		var u:UInt32 = cast 10;
		// UInt32 + Int
		var r = u + 5;
		eq(Std.string(r), "15");
		// UInt32 - Int
		var s = u - 3;
		eq(Std.string(s), "7");
		// UInt32 * Int
		var p = u * 4;
		eq(Std.string(p), "40");
	}

	// --- Regression: previously UInt used sign-bit for comparison ---
	function testSignBitComparison() {
		// A value with the sign bit set should be LARGER than one without it (unsigned)
		var withSign:UInt32 = cast 0x80000001; // 2147483649
		var withoutSign:UInt32 = cast 0x7FFFFFFF; // 2147483647
		t(withSign > withoutSign);
		f(withSign < withoutSign);
		f(withSign == withoutSign);
	}

	// --- Regression: 0 < UInt32.MAX (not just "any bit set is less than 0 boundary") ---
	function testZeroVsMax() {
		t(ZERO < MAX);
		t(MAX > ZERO);
		f(ZERO > MAX);
		f(MAX < ZERO);
	}
}
