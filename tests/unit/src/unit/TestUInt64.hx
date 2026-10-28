package unit;

import haxe.UInt64;
import unit.HelperMacros.typeError;

class TestUInt64 extends Test {
	public function testMake() {
		var a:UInt64;

		a = UInt64.make(0, 42);
		eq(a.high, 0);
		eq(a.low, 42);

		a = UInt64.make(0xFFFFFFFF, 0xFFFFFFFF);
		eq(a.high, 0xFFFFFFFF);
		eq(a.low, 0xFFFFFFFF);

		a = UInt64.make(0x80000000, 0);
		eq(a.high, 0x80000000);
		eq(a.low, 0);

		a = UInt64.make(1, 0);
		eq(a.high, 1);
		eq(a.low, 0);
	}

	public function testOfInt() {
		var a:UInt64;

		a = UInt64.fromInt(0);
		eq(a.high, 0);
		eq(a.low, 0);

		a = UInt64.fromInt(1);
		eq(a.high, 0);
		eq(a.low, 1);

		// Negative int is sign-extended (same bit pattern as Int64)
		a = UInt64.fromInt(-1);
		eq(a.high, 0xFFFFFFFF);
		eq(a.low, 0xFFFFFFFF);
	}

	public function testToInt() {
		eq(UInt64.toInt(UInt64.make(0, 42)), 42);
		eq(UInt64.toInt(UInt64.make(0, 0)), 0);
		// toInt returns low 32 bits even when high is nonzero
		eq(UInt64.toInt(UInt64.make(1, 5)), 5);
		// Negative low word returned as-is
		eq(UInt64.toInt(UInt64.make(0, 0xFFFFFFFF)), 0xFFFFFFFF);
	}

	public function testToString() {
		var a:UInt64;

		a = UInt64.make(0, 0);
		eq(Std.string(a), "0");

		a = UInt64.make(0, 1);
		eq(Std.string(a), "1");

		a = UInt64.make(0, 1000000);
		eq(Std.string(a), "1000000");

		// 2^32 = 4294967296
		a = UInt64.make(1, 0);
		eq(Std.string(a), "4294967296");

		// MAX_UINT64 = 2^64 - 1 = 18446744073709551615
		a = UInt64.make(0xFFFFFFFF, 0xFFFFFFFF);
		eq(Std.string(a), "18446744073709551615");

		// 2^63 = 9223372036854775808 (would be MIN_INT64 in signed)
		a = UInt64.make(0x80000000, 0);
		eq(Std.string(a), "9223372036854775808");

		// 2^63 - 1 = 9223372036854775807
		a = UInt64.make(0x7FFFFFFF, 0xFFFFFFFF);
		eq(Std.string(a), "9223372036854775807");
	}

	public function testComparison() {
		var a:UInt64, b:UInt64;

		// Equal values
		a = UInt64.make(0, 1);
		b = UInt64.make(0, 1);
		t(a == b);
		f(a != b);
		t(a <= b);
		f(a < b);
		t(a >= b);
		f(a > b);
		eq(UInt64.compare(a, b), 0);
		eq(UInt64.ucompare(a, b), 0);

		// Simple ordering
		a = UInt64.make(0, 10);
		b = UInt64.make(0, 20);
		f(a == b);
		t(a != b);
		t(a < b);
		t(a <= b);
		f(a > b);
		f(a >= b);
		t(UInt64.compare(a, b) < 0);
		t(UInt64.ucompare(a, b) < 0);

		// Key unsigned test: 0x80000000_00000000 > 0x7FFFFFFF_FFFFFFFF
		// (In signed Int64, 0x80000000_00000000 would be negative and LESS than 0x7FFFFFFF_FFFFFFFF)
		a = UInt64.make(0x80000000, 0);
		b = UInt64.make(0x7FFFFFFF, 0xFFFFFFFF);
		t(a > b);
		f(a < b);
		f(a == b);
		t(UInt64.compare(a, b) > 0);
		t(UInt64.ucompare(a, b) > 0);

		// MAX > 0
		a = UInt64.make(0xFFFFFFFF, 0xFFFFFFFF);
		b = UInt64.make(0, 0);
		t(a > b);
		f(a < b);

		// High-word comparison
		a = UInt64.make(2, 0);
		b = UInt64.make(1, 0xFFFFFFFF);
		t(a > b);

		// Both have high bit set
		a = UInt64.make(0xFFFFFFFF, 0);
		b = UInt64.make(0x80000000, 0);
		t(a > b);
	}

	public function testAddition() {
		var a:UInt64, b:UInt64;

		a = UInt64.make(0, 100);
		b = UInt64.make(0, 200);
		uint64eq(a + b, UInt64.make(0, 300));

		// Carry from low to high
		a = UInt64.make(0, 0xFFFFFFFF);
		b = UInt64.make(0, 1);
		uint64eq(a + b, UInt64.make(1, 0));

		// Wrap around at 2^64
		a = UInt64.make(0xFFFFFFFF, 0xFFFFFFFF);
		b = UInt64.make(0, 1);
		uint64eq(a + b, UInt64.make(0, 0));

		// UInt64 + Int
		a = UInt64.make(0, 100);
		uint64eq(a + 50, UInt64.make(0, 150));
	}

	public function testSubtraction() {
		var a:UInt64, b:UInt64;

		a = UInt64.make(0, 300);
		b = UInt64.make(0, 100);
		uint64eq(a - b, UInt64.make(0, 200));

		// Borrow from high to low
		a = UInt64.make(1, 0);
		b = UInt64.make(0, 1);
		uint64eq(a - b, UInt64.make(0, 0xFFFFFFFF));

		// Wrap around at 0 (underflow wraps to MAX)
		a = UInt64.make(0, 0);
		b = UInt64.make(0, 1);
		uint64eq(a - b, UInt64.make(0xFFFFFFFF, 0xFFFFFFFF));
	}

	public function testMultiplication() {
		var a:UInt64, b:UInt64;

		a = UInt64.make(0, 1000);
		b = UInt64.make(0, 1000);
		uint64eq(a * b, UInt64.make(0, 1000000));

		// Multiplication with overflow into high word
		a = UInt64.make(0, 0x10000);
		b = UInt64.make(0, 0x10000);
		uint64eq(a * b, UInt64.make(1, 0));

		// UInt64 * Int
		a = UInt64.make(0, 7);
		uint64eq(a * 6, UInt64.make(0, 42));
	}

	public function testDivision() {
		var a:UInt64, b:UInt64;

		// Simple division
		a = UInt64.make(0, 100);
		b = UInt64.make(0, 10);
		uint64eq(a / b, UInt64.make(0, 10));
		uint64eq(a % b, UInt64.make(0, 0));

		// Division with remainder
		a = UInt64.make(0, 103);
		b = UInt64.make(0, 10);
		uint64eq(a / b, UInt64.make(0, 10));
		uint64eq(a % b, UInt64.make(0, 3));

		// Key unsigned test: divide a value > MAX_INT64
		// 2^63 / 2 = 2^62
		a = UInt64.make(0x80000000, 0);
		b = UInt64.make(0, 2);
		uint64eq(a / b, UInt64.make(0x40000000, 0));
		uint64eq(a % b, UInt64.make(0, 0));

		// MAX_UINT64 / 2 = 2^63 - 1 (remainder 1)
		a = UInt64.make(0xFFFFFFFF, 0xFFFFFFFF);
		b = UInt64.make(0, 2);
		uint64eq(a / b, UInt64.make(0x7FFFFFFF, 0xFFFFFFFF));
		uint64eq(a % b, UInt64.make(0, 1));

		// divMod (tested via / and % operators)
		a = UInt64.make(0, 47);
		b = UInt64.make(0, 5);
		uint64eq(a / b, UInt64.make(0, 9));
		uint64eq(a % b, UInt64.make(0, 2));

		// Divide by self
		a = UInt64.make(0x12345678, 0x9ABCDEF0);
		uint64eq(a / a, UInt64.make(0, 1));
		uint64eq(a % a, UInt64.make(0, 0));

		// Divide smaller by larger
		a = UInt64.make(0, 5);
		b = UInt64.make(0, 100);
		uint64eq(a / b, UInt64.make(0, 0));
		uint64eq(a % b, UInt64.make(0, 5));

		// Divide by zero throws
		var threw = false;
		try {
			var _ = UInt64.make(0, 1) / UInt64.make(0, 0);
		} catch (e:Dynamic) {
			threw = true;
		}
		t(threw);

		// Large dividend, large divisor (both with high bit set)
		a = UInt64.make(0xFFFFFFFF, 0xFFFFFFFF); // MAX
		b = UInt64.make(0xFFFFFFFF, 0xFFFFFFFF); // MAX
		uint64eq(a / b, UInt64.make(0, 1));
		uint64eq(a % b, UInt64.make(0, 0));
	}

	public function testBitwiseOps() {
		var a:UInt64, b:UInt64;

		a = UInt64.make(0x0FFFFFFF, 0x00000001);
		b = UInt64.make(0, 0x8FFFFFFF);
		uint64eq(a & b, UInt64.make(0, 1));
		uint64eq(a | b, UInt64.make(0x0FFFFFFF, 0x8FFFFFFF));
		uint64eq(a ^ b, UInt64.make(0x0FFFFFFF, 0x8FFFFFFE));
		uint64eq(~a, UInt64.make(0xF0000000, 0xFFFFFFFE));
	}

	public function testShifts() {
		var a:UInt64;

		a = UInt64.make(0, 1);
		uint64eq(a << 32, UInt64.make(1, 0));
		uint64eq(a << 63, UInt64.make(0x80000000, 0));

		// >> on UInt64 is logical (unsigned) shift, NOT arithmetic
		a = UInt64.make(0x80000000, 0);
		uint64eq(a >> 1, UInt64.make(0x40000000, 0));

		// Verify >> does NOT sign-extend (key unsigned behavior)
		a = UInt64.make(0xFFFFFFFF, 0xFFFFFFFF); // all bits set
		uint64eq(a >> 4, UInt64.make(0x0FFFFFFF, 0xFFFFFFFF));

		// >>> same as >>
		uint64eq(a >>> 4, UInt64.make(0x0FFFFFFF, 0xFFFFFFFF));

		// Shift by 0
		a = UInt64.make(1, 1);
		uint64eq(a << 0, a);
		uint64eq(a >> 0, a);
		uint64eq(a >>> 0, a);
	}

	public function testIncrement() {
		var a:UInt64, b:UInt64;

		a = UInt64.make(0, 0);
		b = a;
		a++;
		f(a == b);
		uint64eq(a, UInt64.make(0, 1));

		a = UInt64.make(0, 0xFFFFFFFF);
		b = a;
		var c = UInt64.make(1, 0);
		uint64eq(a++, b);
		uint64eq(a--, c);
		uint64eq(++a, c);
		uint64eq(--a, b);
	}

	public function testNeg() {
		var a:UInt64;

		a = UInt64.make(0, 1);
		uint64eq(-a, UInt64.make(0xFFFFFFFF, 0xFFFFFFFF)); // -1 == MAX_UINT64

		a = UInt64.make(0, 0);
		uint64eq(-a, UInt64.make(0, 0)); // -0 == 0
	}

	public function testInt64Conversion() {
		// UInt64 <-> Int64 round-trip preserves bits
		var u = UInt64.make(0x80000000, 0x12345678);
		var i:haxe.Int64 = u;
		eq(i.high, 0x80000000);
		eq(i.low, 0x12345678);
		var u2:UInt64 = i;
		t(u == u2);

		// Zero round-trip
		var u0:UInt64 = UInt64.make(0, 0);
		var i0:haxe.Int64 = u0;
		t(i0.isZero());
		uint64eq(u0, i0);
	}

	public function testParseString() {
		eq(Std.string(UInt64.parseString("0")), "0");
		eq(Std.string(UInt64.parseString("1")), "1");
		eq(Std.string(UInt64.parseString("42")), "42");
		eq(Std.string(UInt64.parseString("4294967296")), "4294967296"); // 2^32
		eq(Std.string(UInt64.parseString("9223372036854775807")), "9223372036854775807"); // MAX_INT64
		eq(Std.string(UInt64.parseString("9223372036854775808")), "9223372036854775808"); // MAX_INT64 + 1
		eq(Std.string(UInt64.parseString("18446744073709551615")), "18446744073709551615"); // MAX_UINT64

		// Trims whitespace
		eq(Std.string(UInt64.parseString("  42 ")), "42");

		// Negative throws
		var threw = false;
		try {
			UInt64.parseString("-1");
		} catch (e:Dynamic) {
			threw = true;
		}
		t(threw);

		// Invalid chars throw
		threw = false;
		try {
			UInt64.parseString("abc");
		} catch (e:Dynamic) {
			threw = true;
		}
		t(threw);
	}

	public function testFromFloat() {
		uint64eq(UInt64.fromFloat(0.0), UInt64.make(0, 0));
		uint64eq(UInt64.fromFloat(1.0), UInt64.make(0, 1));
		uint64eq(UInt64.fromFloat(4294967296.0), UInt64.make(1, 0)); // 2^32
		uint64eq(UInt64.fromFloat(9007199254740991.0), UInt64.parseString("9007199254740991")); // 2^53-1

		// Negative throws
		var threw = false;
		try {
			UInt64.fromFloat(-1.0);
		} catch (e:Dynamic) {
			threw = true;
		}
		t(threw);

		// NaN throws
		threw = false;
		try {
			UInt64.fromFloat(Math.NaN);
		} catch (e:Dynamic) {
			threw = true;
		}
		t(threw);
	}

	public function testToFloat() {
		var a:UInt64;

		a = UInt64.make(0, 0);
		feq(a.toFloat(), 0.0);

		a = UInt64.make(0, 42);
		feq(a.toFloat(), 42.0);

		a = UInt64.make(1, 0);
		feq(a.toFloat(), 4294967296.0);

		// Value with high bit set (would be negative in signed)
		a = UInt64.make(0x80000000, 0);
		feq(a.toFloat(), 9223372036854775808.0);
	}

	public function testZero() {
		t(UInt64.make(0, 0).isZero());
		f(UInt64.make(0, 1).isZero());
		f(UInt64.make(1, 0).isZero());
		f(UInt64.make(0xFFFFFFFF, 0xFFFFFFFF).isZero());
	}

	public function testCopy() {
		var a = UInt64.make(0x12345678, 0x9ABCDEF0);
		var b = a.copy();
		t(a == b);
		eq(a.high, b.high);
		eq(a.low, b.low);
	}

	function uint64eq(v:UInt64, v2:UInt64, ?pos:haxe.PosInfos) {
		t(v == v2, pos);
	}

	public function testMinMax() {
		uint64eq(UInt64.MIN, UInt64.make(0, 0));
		uint64eq(UInt64.MAX, UInt64.make(0xFFFFFFFF, 0xFFFFFFFF));

		// MAX + 1 wraps to 0 (MIN)
		uint64eq(UInt64.MAX + UInt64.fromInt(1), UInt64.MIN);
		// MIN - 1 wraps to MAX
		uint64eq(UInt64.MIN - UInt64.fromInt(1), UInt64.MAX);

		eq(Std.string(UInt64.MIN), "0");
		eq(Std.string(UInt64.MAX), "18446744073709551615");
	}

	function testStrictTypeChecking() {
		// Float → UInt64 is not allowed
		t(typeError({var x:haxe.UInt64 = 1.5;}));
		// UInt64 → Float is not allowed (no implicit @:to Float)
		t(typeError({var u:haxe.UInt64 = haxe.UInt64.make(0, 5); var f:Float = u;}));
		// UInt64 → Int32 narrowing is not allowed
		t(typeError({var u:haxe.UInt64 = haxe.UInt64.make(0, 5); var r:haxe.Int32 = u;}));
		// UInt64 → UInt32 narrowing is not allowed
		t(typeError({var u:haxe.UInt64 = haxe.UInt64.make(0, 5); var r:haxe.UInt32 = u;}));
	}
}
