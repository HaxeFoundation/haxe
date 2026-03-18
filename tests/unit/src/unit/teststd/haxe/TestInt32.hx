package unit.teststd.haxe;

import haxe.Int32;
import haxe.UInt32;

class TestInt32 extends unit.Test {
	// --- Constants ---
	static var MAX:Int32 = Int32.MAX;
	static var MIN:Int32 = Int32.MIN;
	static var ZERO:Int32 = 0;
	static var ONE:Int32 = 1;
	static var NEG_ONE:Int32 = -1;

	function testMinMax() {
		eq((Int32.MAX : Int32), (0x7fffffff : Int32));
		eq((Int32.MIN : Int32), (0x80000000 : Int32));

		// MIN/MAX constants match expected values
		var max:haxe.Int32 = 0x7fffffff;
		var min:haxe.Int32 = 0x80000000;
		eq(haxe.Int32.MAX, max);
		eq(haxe.Int32.MIN, min);
	}

	// --- Overflow behavior ---
	function testOverflowAdd() {
		eq((MAX + ONE : Int32), MIN);
		eq((MIN + NEG_ONE : Int32), MAX);
		eq((MAX + MIN : Int32), NEG_ONE);
		eq((MAX + 1 : Int32), MIN);
	}

	function testOverflowSub() {
		eq((MIN - ONE : Int32), MAX);
		eq((MAX - MIN : Int32), NEG_ONE);
		eq((MIN - 1 : Int32), MAX);
	}

	function testOverflowMul() {
		eq((MAX * MAX : Int32), ONE);
		eq((MAX * 2 : Int32), cast(-2, Int32));
		var minVal:Int32 = MIN;
		eq((MAX * minVal : Int32), MIN);
	}

	function testOverflowNeg() {
		// Two's complement: -MIN overflows back to MIN
		eq((-MIN : Int32), MIN);
		// Normal negation
		eq((-ONE : Int32), NEG_ONE);
		eq((-NEG_ONE : Int32), ONE);
		eq((-ZERO : Int32), ZERO);
	}

	// --- Increment/Decrement ---
	function testPreIncrement() {
		var a:haxe.Int32 = 0x7fffffff;
		var max:haxe.Int32 = 0x7fffffff;
		var min:haxe.Int32 = 0x80000000;
		eq(a++, max);
		eq(a, min);
		eq(a--, min);
		eq(a, max);
		eq(++a, min);
		eq(--a, max);

		var b:Int32 = MAX;
		eq(++b, MIN);
		eq(b, MIN);
	}

	function testPostIncrement() {
		var a:Int32 = MAX;
		eq(a++, MAX);
		eq(a, MIN);
	}

	function testPreDecrement() {
		var a:Int32 = MIN;
		eq(--a, MAX);
		eq(a, MAX);
	}

	function testPostDecrement() {
		var a:Int32 = MIN;
		eq(a--, MIN);
		eq(a, MAX);
	}

	// --- Bitwise operations ---
	function testComplement() {
		eq((~ZERO : Int32), NEG_ONE);
		eq((~NEG_ONE : Int32), ZERO);
		eq((~MAX : Int32), MIN);
		eq((~MIN : Int32), MAX);
	}

	function testBitwiseAnd() {
		eq((MAX & MIN : Int32), ZERO);
		eq((NEG_ONE & MAX : Int32), MAX);
		eq((NEG_ONE & MIN : Int32), MIN);
	}

	function testBitwiseOr() {
		var expected:Int32 = NEG_ONE;
		eq((MAX | MIN : Int32), expected);
		eq((ZERO | MAX : Int32), MAX);
	}

	function testBitwiseXor() {
		var expected:Int32 = NEG_ONE;
		eq((MAX ^ MIN : Int32), expected);
		eq((MAX ^ MAX : Int32), ZERO);
		eq((MIN ^ MIN : Int32), ZERO);
	}

	// --- Shift operations ---
	function testShiftLeft() {
		var one:Int32 = 1;
		eq((one << 31 : Int32), MIN);
		eq((MIN << 1 : Int32), ZERO);
		var v:Int32 = 0xFF;
		eq((v << 8 : Int32), cast(0xFF00, Int32));

		var min:haxe.Int32 = 0x80000000;
		eq(min << 1, 0);
	}

	function testShiftRight() {
		eq((MIN >> 1 : Int32), cast(0xc0000000, Int32));
		eq((NEG_ONE >> 1 : Int32), NEG_ONE); // sign extension
		eq((MAX >> 1 : Int32), cast(0x3fffffff, Int32));

		var min:haxe.Int32 = 0x80000000;
		eq(min >> 1, 0xc0000000);
	}

	function testUnsignedShiftRight() {
		eq((MIN >>> 1 : Int32), cast(0x40000000, Int32));
		eq((NEG_ONE >>> 1 : Int32), MAX);

		var min:haxe.Int32 = 0x80000000;
		eq(min >>> 1, 0x40000000);
	}

	// --- Unsigned comparison ---
	function testUcompare() {
		// 0 < MAX (unsigned)
		t(Int32.ucompare(ZERO, MAX) < 0);
		// MAX < MIN (unsigned, since MIN = 0x80000000 is large unsigned)
		t(Int32.ucompare(MAX, MIN) < 0);
		// MIN < NEG_ONE (unsigned, 0x80000000 < 0xFFFFFFFF)
		t(Int32.ucompare(MIN, NEG_ONE) < 0);
		// Equal values
		eq(Int32.ucompare(MAX, MAX), 0);
		eq(Int32.ucompare(MIN, MIN), 0);
		eq(Int32.ucompare(ZERO, ZERO), 0);
	}

	// --- Comparison operators ---
	function testComparison() {
		t(MIN < MAX);
		t(MAX > MIN);
		t(MIN <= MIN);
		t(MAX >= MAX);
		t(ZERO == ZERO);
		t(ONE != ZERO);
	}

	// --- Mixed-type operations ---
	function testMixedIntOps() {
		// Int32 + Int
		eq((MAX + 1 : Int32), MIN);
		// Int32 - Int
		eq((MIN - 1 : Int32), MAX);
		// Int - Int32
		eq((0 - ONE : Int32), NEG_ONE);
	}

	function testMixedFloatOps() {
		// Int32 + Float returns Float
		var result:Float = MAX + 0.5;
		feq(result, 2147483647.5);
		// Int32 * Float returns Float
		var result2:Float = ONE * 2.5;
		feq(result2, 2.5);
	}

	// --- Conversion ---
	function testToFloat() {
		var f:Float = MAX;
		feq(f, 2147483647.0);
		var f2:Float = MIN;
		feq(f2, -2147483648.0);
	}

	function testFromInt() {
		var a:Int32 = 42;
		eq((a : Int), 42);
		var b:Int32 = -42;
		eq((b : Int), -42);
	}

	// --- Division (returns Float) ---
	function testDivision() {
		var ten:Int32 = 10;
		var three:Int32 = 3;
		feq((ten : Float) / (three : Float), 10.0 / 3.0);
	}

	// --- Modulus ---
	function testModulus() {
		var ten:Int32 = 10;
		var three:Int32 = 3;
		eq((ten % three : Int32), cast(1, Int32));
		var negTen:Int32 = -10;
		eq((negTen % three : Int32), cast(-1, Int32));
	}

	// --- Specific regression tests ---
	function testTwosComplementOverflow_Issue7491() {
		// https://github.com/HaxeFoundation/haxe/pull/7491
		var min:Int32 = MIN;
		eq(-min, min); // two's complement overflow
		eq(-2147483643, cast(5 + -min, Int)); // order of ops and negate
		eq(2147483643, cast(-(5 + min), Int)); // static analyzer issue

		// Old test from teststd (uses equals form)
		-min == min;              // two's complement overflow,
		-2147483643 == 5 + -min;  // order of ops and negate
		2147483643 == -(5 + min); // static analyzer issue
	}

	// https://github.com/HaxeFoundation/haxe/issues/10780
	// C++ inconsistent overflow behavior for Int32 with multiplication
	function testMulOverflow_Issue10780() {
		var a:Int32 = 257;
		var b:Int32 = 0x01010101;
		// 257 * 0x01010101 = 0x102020201 overflows 32-bit to 0x02020201 = 33686017
		eq((a * b : Int), 0x02020201);
	}

	// https://github.com/HaxeFoundation/haxe/issues/10995
	// Python: wrong result when XOR with dynamic shift amount
	function testXorWithDynamicShift_Issue10995() {
		var changeBit = 31;
		var result:Int32 = MIN ^ (1 << changeBit);
		eq((result : Int), 0);
	}

	// https://github.com/HaxeFoundation/haxe/issues/5938
	// Python: |= not clamping result to 32 bits
	function testOrEqualsNotClamping_Issue5938() {
		var i32:Int32 = 15459750;
		var arr = [178, 0, 0];
		var next = 0;
		i32 |= (arr[next] << 24);
		eq((i32 : Int), -1293163098);
	}

	// C++ handles array indexing with Int32 differently due to native type handling
	#if !cpp
	function testArrayIndexWithInt32() {
		var a = [1];
		var next = 0;

		var i32:Int32 = MAX - 1;
		i32 |= ((a[next] << 32) | 1);
		eq(i32, MAX);

		var i32:Int32 = ((a[next] << 33) | 3);
		i32 >>= 1;
		eq((i32 : Int), 1);

		var i32:Int32 = 2;
		i32 ^= (((a[next] << 32) | 1) : Int32);
		eq((i32 : Int), 3);

		var i32:Int32 = 2;
		var c = ~(((a[next] << 32) | 1) : Int32);
		eq(c, cast(0xfffffffe, Int32));
	}
	#end

	// --- Arithmetic identity tests ---
	function testArithmeticIdentities() {
		var a:Int32 = 12345;
		eq((a + ZERO : Int32), a);
		eq((a - ZERO : Int32), a);
		eq((a * ONE : Int32), a);
		eq(a, a);
	}

	function testBitwiseIdentities() {
		var a:Int32 = 0xDEADBEEF;
		eq((a & NEG_ONE : Int32), a);
		eq((a | ZERO : Int32), a);
		eq((a ^ ZERO : Int32), a);
		eq((a ^ a : Int32), ZERO);
	}

	#if hl
	function testHlEdgeCases() {
		var min:Int32 = MIN;
		var max:Int32 = MAX;
		eq(0, min % 0);              // % 0 div by zero exception
		eq(0, Std.int(min / 0));
		eq(0, min % -1);             // min % -1 integer overflow exception
		eq(min, Std.int(min / -1));
		eq(min, min * -1);
		eq(0, min % 1);
		eq(0, max % 0);
		eq(0, Std.int(max / 0));
		eq(0, max % -1);
		eq(-max, Std.int(max / -1));
		eq(-max, max * -1);
		eq(0, max % 1);
	}
	#end
}
