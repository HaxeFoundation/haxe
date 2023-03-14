package unit;

import haxe.Int64.*;

using haxe.Int64;

class TestInt64 extends Test {

	public function testMake() {
		var a : Int64, i : Int;

		// Test creation and fields
		a = Int64.make(10,0xFFFFFFFF);
		eq( a.high, 10 );
		eq( a.low, 0xFFFFFFFF );

		a = 47244640255i64;
		eq( a.high, 10 );
		eq( a.low, 0xFFFFFFFF );

		a = 0x7FFFFFFFFFFFFFFFi64;
		eq( a.high, 0x7FFFFFFF );
		eq( a.low, 0xFFFFFFFF );

		// Int casts
		a = 1;
		eq( a.toInt(), 1 );

		a = -1;
		eq( a.high, 0xFFFFFFFF );
		eq( a.low, 0xFFFFFFFF );
		eq( a.toInt(), -1 );

		a = Int64.make(0,0x80000000);
		eq( a.high, 0 );
		eq( a.low, 0x80000000 );
		exc( tryOverflow.bind(a) );	// Throws Overflow

		a = Int64.make(0xFFFFFFFF,0x80000000);
		eq( a.high, 0xFFFFFFFF );
		eq( a.low, 0x80000000 );
		eq( a.toInt(), -2147483648 );

		a = Int64.make(0xFFFFFFFF,0x7FFFFFFF);
		eq( a.high, 0xFFFFFFFF );
		eq( a.low, 0x7FFFFFFF );
		exc( tryOverflow.bind(a) );	// Throws Overflow
	}

	public function testNegateOverflow_Issue7485()
	{
		// Guarantee two's complement overflow (-min == min)
		//  - discussion: https://github.com/HaxeFoundation/haxe/pull/7491
		var min = haxe.Int64.parseString('-9223372036854775808');
		eq( min==(-min), true);

		// 0x7fffffff - 0x7fffffffffffffff = -7fffffff80000000
		var a = haxe.Int64.parseString('2147483647');
		var b = haxe.Int64.parseString('9223372036854775807');
		var z = haxe.Int64.sub(a, b);
		eq(haxe.Int64.toStr(z), "-9223372034707292160");

		// This fails because the first division fails:
		var ten = haxe.Int64.make(0, 10);
		var modulus = haxe.Int64.divMod(z, ten).modulus.low;
		eq(modulus, 0);

		// The first division failed because of negate:
		eq((-z).low, -2147483648);
		eq((-z).high, 2147483647);
	}

	// Some tests of how it generates Int64 when used as Null<Int64> or as type parameters
	function testGen() {
		var arr:Array<Int64> = [];
		arr.push(1);
		arr.push(Int64.make(0xFFFFFFFF, 0x80000000));
		eq(arr[0].high, 0);
		eq(arr[0].low, 1);
		eq(arr[1].high, 0xFFFFFFFF);
		eq(arr[1].low, 0x80000000);

		var n:Null<Int64> = null;
		eq(n, null);
		var dyn:Dynamic = n;
		eq(dyn, null);
		n = Int64.make(0xf0f0f0f0, 0xefefefef);
		eq(n.high, 0xf0f0f0f0);
		eq(n.low, 0xefefefef);
	}

	function tryOverflow(a:Int64) {
		a.toInt();
	}

	public function testIncrement() {
		var a:Int64, b:Int64, c:Int64;

		// Int64 should act as a value type and be immutable.
		// Increment ops should swap `this` to a new Int64 object.
		a = 0;
		b = a;
		a++;
		f(a == b);

		a = 0;
		b = a;
		++a;
		f(a == b);

		a = 0;
		b = a;
		a--;
		f(a == b);

		a = 0;
		b = a;
		--a;
		f(a == b);

		a = Int64.make(0, 0xFFFFFFFF);
		b = a;
		c = Int64.make(1, 0);
		int64eq(a++, b);
		int64eq(a--, c);
		int64eq(++a, c);
		int64eq(--a, b);
	}

	public function testToString() {
		var a:Int64;

		// Issue #903
		a = 0;
		eq(Std.string(a), "0");

		a = 156;
		eq('$a', "156");

		a = -1;
		eq('$a', "-1");

		a = Int64.make(0xFFFFFFFE, 0);
		eq(a.toStr(), "-8589934592");

		a = Int64.make(1, 1);
		eq(a.toStr(), "4294967297");

		// set a to 2^63 (overflows to the smallest negative number)
		a = Int64.ofInt(2);
		for (i in 0...62) {
			a = Int64.mul(a, 2);
		}

		eq(Int64.add(a, -1).toStr(), "9223372036854775807"); // largest positive
		eq(Int64.add(a, 1).toStr(), "-9223372036854775807"); // smallest negative - 1
		eq(a.toStr(), "-9223372036854775808"); // smallest negative
	}

	public function testComparison() {
		var a:Int64, b:Int64;
		a = 1;
		b = 1;
		t(a == b);
		f(a != b);
		t(a <= b);
		f(a < b);
		t(a >= b);
		f(a > b);
		eq(a.compare(b), 0);
		eq(a.ucompare(b), 0);

		a = -10;
		b = -15;
		f(a == b);
		t(a != b);
		f(a <= b);
		f(a < b);
		t(a >= b);
		t(a > b);
		t(a.compare(b) > 0);
		t(a.ucompare(b) > 0);

		a = Int64.make(0, 0);
		b = Int64.make(1, 0);
		f(a == b);
		t(a != b);
		t(a <= b);
		t(a < b);
		f(a >= b);
		f(a > b);
		t(a.compare(b) < 0);
		t(a.ucompare(b) < 0);

		a = Int64.make(0x0FFFFFFF, 0xFFFFFFFF);
		b = Int64.make(0x80000000, 0x80000000);
		f(a == b);
		t(a != b);
		f(a <= b);
		f(a < b);
		t(a >= b);
		t(a > b);
		t(a.compare(b) > 0);
		t(a.ucompare(b) < 0);

		// Issue #2317
		a = Int64.make(0xA, 0x828D97A8);
		b = 0;
		t(a.compare(b) > 0);
		t(a.ucompare(b) > 0);

		// Issue #2090
		a = 0;
		b = Int64.make(320, -2138504556);
		t(a.compare(b) < 0);
		t(a.ucompare(b) < 0);
	}

	public function testAddition() {
		var a:Int64, b:Int64;

		a = Int64.make(0, 0x9E370301);
		b = Int64.make(0, 0xB0590000);
		int64eq(a + b, Int64.make(0x1, 0x4E900301));

		// negation
		b = -a;
		int64eq(a + b, 0);

		// Int64+Int
		int64eq(a + 12345678, Int64.make(0, 0x9EF3644F));

		// Int+Int64
		int64eq(0xF0002222 + a, Int64.make(0, 0x8E372523));
	}

	public function testSubtraction() {
		var a:Int64, b:Int64;

		a = Int64.make(1, 0);
		b = Int64.make(0, 1);
		int64eq(a - b, Int64.make(0, 0xFFFFFFFF));

		b = a;
		int64eq(a - b, 0);

		b = Int64.make(0xFFFFFFFE, 1);
		int64eq(a - b, Int64.make(2, 0xFFFFFFFF));

		// Int64-Int
		int64eq(a - 12345678, Int64.make(0, 0xFF439EB2));

		// Int-Int64
		int64eq(12345678 - a, Int64.make(0xFFFFFFFF, 0x00BC614E));
	}

	public function testMultiplication() {
		var a:Int64, b:Int64;

		a = Int64.make(0, 0x239B0E13);
		b = Int64.make(0, 0x39193D1B);
		int64eq(a * b, Int64.make(0x07F108C6, 0x4E900301));

		a = Int64.make(0, 0xD3F9C9F4);
		b = Int64.make(0, 0xC865C765);
		int64eq(a * b, Int64.make(0xA5EF6C6E, 0x1ACD5944));

		var a = Int64.make(0xFFF21CDA, 0x972E8BA3);
		var b = Int64.make(0x0098C29B, 0x81000001);
		int64eq(a * b, Int64.make(0xDDE8A2E8, 0xBA2E8BA3));

		var a = Int64.make(0x81230000, 0x81230000);
		var b = Int64.make(0x90909090, 0x90909090);
		int64eq(a * b, Int64.make(0xF04C9C9C, 0x53B00000));

		var a = Int64.make(0x00001000, 0x0020020E);
		var b = Int64.make(0xBEDBEDED, 0xDEBDEBDE);
		int64eq(a * b, Int64.make(0xC45C967D, 0x25FAA224));

		// Issue #1532
		a = Int64.make(0xFFF21CDA, 0x972E8BA3);
		b = Int64.make(0x0098C29B, 0x81000001);
		int64eq(a * b, Int64.make(0xDDE8A2E8, 0xBA2E8BA3));

		// Int64*Int
		a = Int64.make(0x01000000, 0x11131111);
		int64eq(a * 7, Int64.make(0x07000000, 0x77857777));

		// Int*Int64
		a = Int64.make(0x91111111, 0x11111111);
		int64eq(2 * a, Int64.make(0x22222222, 0x22222222));
	}

	public function testDivision() {
		var a:Int64, b:Int64;

		a = Int64.make(0x00002342, 0xDDEF3421);
		b = Int64.make(0x00000001, 0x00002000);
		int64eq(a / b, 9026);
		int64eq(a % b, Int64.make(0, 0xD986F421));
		var result = a.divMod(b);
		int64eq(a / b, result.quotient);
		int64eq(a % b, result.modulus);

		a = Int64.make(0xF0120AAA, 0xAAAAAAA0);
		b = Int64.make(0x00020001, 0x0FF02000);
		int64eq(a / b, -2038);
		int64eq(a % b, Int64.make(0xFFFE131F, 0x8C496AA0));
		result = a.divMod(b);
		int64eq(a / b, result.quotient);
		int64eq(a % b, result.modulus);

		a = Int64.make(0, 2);
		b = Int64.make(0xFFFFFFFF, 0xFAFAFAFA);
		int64eq(a / b, 0);
		int64eq(a % b, 2);
		result = a.divMod(b);
		int64eq(a / b, result.quotient);
		int64eq(a % b, result.modulus);

		a = Int64.make(0x7ABADDAD, 0xDEADBEEF);
		b = Int64.make(0xFFFFFFF1, 0x1FFFFFFF);
		int64eq(a / b, Int64.make(0xFFFFFFFF, 0xF7BFCEAE));
		int64eq(a % b, Int64.make(0x0000000A, 0x166D8D9D));
		result = a.divMod(b);
		int64eq(a / b, result.quotient);
		int64eq(a % b, result.modulus);

		a = Int64.make(0x81234567, 0xFDECBA98);
		b = Int64.make(0xFFFFFEFF, 0xEEEEEEEE);
		int64eq(a / b, 0x007ED446);
		int64eq(a % b, Int64.make(0xFFFFFFF5, 0x31964D84));
		result = a.divMod(b);
		int64eq(a / b, result.quotient);
		int64eq(a % b, result.modulus);

		// Int64/Int
		int64eq(a / 2, Int64.make(0xC091A2B3, 0xFEF65D4C));
		int64eq(a % 100, -68);

		// Int/Int64
		int64eq(10001 / a, 0);
		int64eq(515151 % a, 515151);
	}

	public function testBinaryOps() {
		var a:Int64, b:Int64;

		a = haxe.Int64.make(0x0FFFFFFF, 0x00000001);
		b = haxe.Int64.make(0, 0x8FFFFFFF);
		int64eq(a & b, 1);
		int64eq(a | b, Int64.make(0x0FFFFFFF, 0x8FFFFFFF));
		int64eq(a ^ b, Int64.make(0x0FFFFFFF, 0x8FFFFFFE));
		int64eq(~a, Int64.make(0xF0000000, 0xFFFFFFFE));
	}

	public function testShifts() {
		var a:Int64;

		a = 1 << 20;
		int64eq(a, 0x100000);

		a <<= 20;
		int64eq(a, Int64.make(0x100, 0x00000000));

		a = -1;
		a >>= 4;
		int64eq(a, -1);
		a >>>= 4;
		int64eq(a, Int64.make(0x0FFFFFFF, 0xFFFFFFFF));

		// Ensure proper overflow behavior for shift operand
		a = Int64.make(1, 1);
		int64eq(a << 0, a);
		int64eq(a >> 0, a);
		int64eq(a >>> 0, a);
		int64eq(a << 64, a);
		int64eq(a >> 64, a);
		int64eq(a >>> 64, a);
	}

	/** Tests that we have all of the classic Int64 interface. */
	public function testBackwardsCompat() {
		var a:Int64 = 32.ofInt();
		var b:Int64 = (-4).ofInt();

		f(a.eq(b));
		t(a.neq(b));
		int64eq(a.add(b), 28);
		int64eq(a.sub(b), 36);
		int64eq(a.div(b), -8);
		int64eq(a.mod(b), 0);
		int64eq(b.shl(1), -8);
		int64eq(b.shr(1), -2);
		int64eq(b.ushr(1), Int64.make(0x7FFFFFFF, 0xFFFFFFFE));
		int64eq(a.and(b), 32);
		int64eq(a.or(b), -4);
		int64eq(a.xor(b), -36);
		int64eq(a.neg(), -32);
		f(a.isNeg());
		t(b.isNeg());
		f(a.isZero());
		f(b.isZero());
		t(a.compare(b) > 0);
		t(a.ucompare(b) < 0);
		int64eq(a.toInt(), 32);
		int64eq(b.toInt(), -4);
	}

	public function testCapture() {
		var a = Int64.make(0xFF00FF00, 0xF0F0F0F0),
			b = Int64.make(0xFF00FF00, 0xF0F0F0F0);
		eq(a.compare(b), 0);
		eq(a.high, 0xFF00FF00);
		function test()
			return Int64.compare(a, Int64.make(0xFF00FF00, 0xF0F0F0F0));
		eq(test(), 0);
		function testSet(v:Int64)
			b = v;
		testSet(make(0xFF00FF00, 0xFF0));
		eq(b.compare(make(0xFF00FF00, 0xFF0)), 0);
		eq(b.high, 0xFF00FF00);
	}

	public function testMath() {
		var a = Int64.make(0, 0x239B0E13);
		var b = Int64.make(0, 0x39193D1B);
		var c = Int64.mul(a, b);
		eq(c.toStr(), "572248275467371265");
		eq(Int64.toStr(c), "572248275467371265");

		var a = Int64.make(0, 0xD3F9C9F4);
		var b = Int64.make(0, 0xC865C765);
		var c = Int64.mul(a, b);
		eq(c.toStr(), "-6489849317865727676");

		var a = Int64.make(0, 0x9E370301);
		var b = Int64.make(0, 0xB0590000);
		var c = Int64.add(a, b);
		eq(Int64.toStr(c), "5613028097");

		var a = Int64.make(0xFFF21CDA, 0x972E8BA3);
		var b = Int64.make(0x0098C29B, 0x81000001);
		var c = Int64.mul(a, b);
		var expected = Int64.make(0xDDE8A2E8, 0xBA2E8BA3);
		eq(expected.compare(c), 0);
	}

	public function testCompare() {
		var a = ofInt(2), b = ofInt(3);
		t(a == a);
		t(b == b);
		eq(a.compare(a), 0);
		eq(a.compare(b), -1);
		eq(b.compare(a), 1);
	}

	public function testBits() {
		var x = make(0xfedcba98, 0x76543210);
		var y = x.and((ofInt(0xffff))),
			z = x.or((ofInt(0xffff))),
			w = x.xor((make(0xffffffff, 0xffffffff)));
		eq(y.toStr(), '12816');
		eq(z.toStr(), '-81985529216434177');
		eq(w.toStr(), '81985529216486895');
		eq(x.and(ofInt(0xffff)).toStr(), '12816');
		eq((x.or(ofInt(0xffff))).toStr(), '-81985529216434177');
		eq((x.xor(ofInt(0xffff))).toStr(), '-81985529216446993');
		eq((x.and(make(0x1, 0xffffffff))).toStr(), '1985229328');
		eq((x.or(make(0x1, 0xffffffff))).toStr(), '-81985522611781633');
		eq((x.xor(make(0x1, 0xffffffff))).toStr(), '-81985524597010961');
		var a = ofInt(7), b = a.shl(1);
		eq(b.toStr(), '14');
	}

	public function testAdd() {
		var a = ofInt(3), b = ofInt(2), c = make(0xffffffff, 0xfffffffe);
		eq((a.add(b)).compare(ofInt(5)), 0);
		eq((a.add(ofInt(4))).compare(ofInt(7)), 0);
		eq((c.add(ofInt(3))).compare(ofInt(1)), 0);
		// numbers larger than int32
		eq(a.add(make(0x1, 0)).toStr(), '4294967299');
	}

	public function testNeg() {
		eq(Std.string(ofInt(-1)), Std.string(neg(ofInt(1))));
		eq(Std.string(ofInt(-100)), Std.string(neg(ofInt(100))));
		eq(Std.string(make(-2147483648, 1)), Std.string(neg(make(2147483647, -1)))); // -9223372036854775807 == neg(9223372036854775807)
	}

	function int64eq(v:Int64, v2:Int64, ?pos) {
		t(v == v2);
	}

	public function testParseString() {
		for (v in ["0", "1", "-1", "9223372036854775807", "-9223372036854775807"]) {
			eq(Std.string(parseString(v)), v);
		}

		// trims the string:
		eq("-23", Std.string(parseString("  -23 ")));

		// overflow and underflow raise exceptions:
		try {
			parseString("9223372036854775808");
			f(true);
		} catch (e:Dynamic) {
			// fine
		}

		try {
			parseString("-9223372036854775809");
			f(true);
		} catch (e:Dynamic) {
			// fine
		}

		try {
			parseString("--1");
			f(true);
		} catch (e:Dynamic) {
			// fine
		}

		try {
			parseString("asd1");
			f(true);
		} catch (e:Dynamic) {
			// fine
		}

		try {
			parseString("1asdf");
			f(true);
		} catch (e:Dynamic) {
			// fine
		}
	}

	public function testFromFloat() {
		for (v in [0.0, 1.0, -1.0, 9007199254740991, -9007199254740991]) {
			eq(Std.parseFloat(Std.string(fromFloat(v))), v);
		}

		try {
			fromFloat(9007199254740992);
			f(true);
		} catch (e:Dynamic) {
			// fine
		}

		try {
			fromFloat(-9007199254740992);
			f(true);
		} catch (e:Dynamic) {
			// fine
		}

		var nan = Math.NaN;
		try {
			fromFloat(nan);
			f(true);
		} catch (e:Dynamic) {
			// fine
		}
	}

	static function toHex(v:haxe.Int64) {
		return "0x" + (v.high == 0 ? StringTools.hex(v.low) : StringTools.hex(v.high) + StringTools.hex(v.low, 8));
	}

	function testNicolas() {
		inline function check(a:haxe.Int64, str:String) {
			eq(toHex(a), str);
		}

		check(33, "0x21");
		check(-8, "0xFFFFFFFFFFFFFFF8");
		var x = haxe.Int64.make(0x87654321, 0xABCDEF99);
		check(x, "0x87654321ABCDEF99");
		var y = haxe.Int64.make(0xABCDEF99, 0x87654321);
		var small = haxe.Int64.make(0x3, 0x87654321);
		check(x + x, "0xECA8643579BDF32");
		check(x * x, "0xF6889F3201490971");
		check(x + y, "0x333332BB333332BA");
		check(x - y, "0xDB9753882468AC78");
		check(x / small, "0xFFFFFFFFDDD2DC89");
		check(x % small, "0xFFFFFFFFA2DAA6F0");
		check(-x, "0x789ABCDE54321067");
		check(small / 255, "0x38AF033");
		check(small % 255, "0x54");
		check(x << 1, "0xECA8643579BDF32");
		check(x << 33, "0x579BDF3200000000");
		check(x >> 8, "0xFF87654321ABCDEF");
		check(x >> 40, "0xFFFFFFFFFF876543");
		check(x >>> 40, "0x876543");
		check(x & y, "0x8345430183454301");
		check(x | y, "0xAFEDEFB9AFEDEFB9");
		check(x ^ y, "0x2CA8ACB82CA8ACB8");
		x++;
		check(x, "0x87654321ABCDEF9A");
		x--;
		check(x, "0x87654321ABCDEF99");
		check(~x, "0x789ABCDE54321066");
		t(x == x);
		f(x == y);

		t(y >= x);
		f(x >= y);
		t(x >= x);
		t(x + 1 >= x);
		f(x - 1 >= x);
		t(y > x);
		f(x > y);
		t(x + 1 > x);
		f(x - 1 > x);
		f(x > x);

		f(y <= x);
		t(x <= y);
		t(x <= x);
		f(x + 1 <= x);
		t(x - 1 <= x);
		f(y < x);
		t(x < y);
		f(x + 1 < x);
		t(x - 1 < x);
		f(x < x);
	}
}
