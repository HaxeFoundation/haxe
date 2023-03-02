package unit;

import haxe.PosInfos;
import haxe.math.bigint.MultiwordArithmetic;
import haxe.math.bigint.MutableBigInt_;
import haxe.math.bigint.BigInt_;
import haxe.ds.Vector;
import haxe.io.Bytes;
import haxe.math.bigint.BigIntArithmetic;
import haxe.math.bigint.BigIntExceptions;
import haxe.math.bigint.BigIntTools;
import haxe.math.bigint.MutableBigInt;
import haxe.math.bigint.BigInt;
import haxe.math.bigint.BigIntHelper;

class TestBigInt extends Test {

	public function testBigInt():Void {
		MutableBigInt_.s_testAllocation = false;
		bigIntAllChecks();

		MutableBigInt_.s_testAllocation = true;
		MutableBigInt_.s_debugAllocationPadding = 0;
		bigIntAllChecks();

		MutableBigInt_.s_testAllocation = true;
		MutableBigInt_.s_debugAllocationPadding = 1;
		bigIntAllChecks();
	}

	private function bigIntAllChecks():Void {
		bigIntWorkOverlapsDividendDoesntDestroyDividend();
		bigIntSetFromBigEndianBytesUnsignedWithLengthAndOffset();
		bigIntSetFromBytesUnsigned();
		bigIntSetFromUnsignedIntsReusing();
		bigIntSetFromUnsignedInts();
		bigIntCompare();
		bigIntMultiplicationAndDivisionSemantics();
		bigIntMultiplicationAndDivision();
		bigIntEquality();
		bigIntAdditionAndSubtraction();
		bigIntAddAssignDoesntClobber();
		bigIntNegate();
		bigIntFromString();
		bigIntFromBytes();
		bigIntArithmeticShiftLeftAssignDoesntClobber();
		bigIntArithmeticShiftRightAssignDoesntClobber();
		bigIntArithmeticShiftLeft();
		bigIntIsZero();
		bigIntSign();
		bigIntSetFromIntWithLargeLength();
		bigIntHexStrings();
		bigIntDecimalStrings();
	}

	public function bigIntWorkOverlapsDividendDoesntDestroyDividend():Void {
		// Test work overlaps dividend doesn't destroy dividend
		var dividend:MutableBigInt = 0;
		var divisor:MutableBigInt = 1;
		var quotient:MutableBigInt = 0;
		var remainder:MutableBigInt = 0;
		try {
			BigIntArithmetic.divide(dividend, divisor, quotient, remainder, dividend);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		eq("00000000", dividend.toHex());
	}

	public function bigIntSetFromBigEndianBytesUnsignedWithLengthAndOffset():Void {
		// test set from big endian by test unsigned with length and offset
		var t:MutableBigInt = 0;
		t.setFromBigEndianBytesUnsigned(Bytes.ofHex("812345"), 0, 1);
		eq("129", t.toString());
		t.setFromBigEndianBytesUnsigned(Bytes.ofHex("812345"), 1, 1);
		eq("35", t.toString());
	}

	public function bigIntSetFromBytesUnsigned():Void {
		// test set from bytes unsigned
		eq(BigInt.ZERO.toHex(), MutableBigInt.fromBigEndianBytesUnsigned(Bytes.alloc(0)).toHex());
		eq(BigInt.ZERO.toHex(), MutableBigInt.fromLittleEndianBytesUnsigned(Bytes.alloc(0)).toHex());
		checkSetFromBytesUnsigned("81");
		checkSetFromBytesUnsigned("8123");
		checkSetFromBytesUnsigned("812345");
		checkSetFromBytesUnsigned("81234567");
		checkSetFromBytesUnsigned("8123456789");
		checkSetFromBytesUnsigned("8123456789ab");
		checkSetFromBytesUnsigned("8123456789abcd");
		checkSetFromBytesUnsigned("8123456789abcdef");
		checkSetFromBytesUnsigned("8123456789abcdef01");
		checkSetFromBytesUnsigned("8123456789abcdef0123");
		checkSetFromBytesUnsigned("8123456789abcdef012345");
		checkSetFromBytesUnsigned("8123456789abcdef01234567");

		checkSetFromBytesUnsigned("572e4794");
	}

	public function bigIntSetFromUnsignedIntsReusing():Void {
		// set from unsigned ints reusing
		var t1:MutableBigInt = 0;
		var v1 = new Vector<Int>(3);
		v1[0] = 0;
		v1[1] = 1;
		v1[2] = -2147483648;
		t1.setFromUnsignedInts(v1, 3);
		eq(BigInt.fromHexUnsigned("80000000 00000001 00000000").toString(), t1.toString());
		v1[1] = -2147483648;
		t1.setFromUnsignedInts(v1, 2);
		eq(BigInt.fromHexUnsigned("80000000 00000000").toString(), t1.toString());
	}

	function bigIntSetFromUnsignedInts():Void {
		// set from unsigned ints
		checkSetFromUnsignedInts("00000000", [0]);
		checkSetFromUnsignedInts("7fffffff", [2147483647]);
		checkSetFromUnsignedInts("80000000", [-2147483648]);
		checkSetFromUnsignedInts("ffffffff", [-1]);

		checkSetFromUnsignedInts("00000000 55555555", [1431655765, 0]);
		checkSetFromUnsignedInts("7fffffff 55555555", [1431655765, 2147483647]);
		checkSetFromUnsignedInts("80000000 55555555", [1431655765, -2147483648]);
		checkSetFromUnsignedInts("ffffffff 55555555", [1431655765, -1]);

		checkSetFromUnsignedInts("00000000 aaaaaaaa", [-1431655766, 0]);
		checkSetFromUnsignedInts("7fffffff aaaaaaaa", [-1431655766, 2147483647]);
		checkSetFromUnsignedInts("80000000 aaaaaaaa", [-1431655766, -2147483648]);
		checkSetFromUnsignedInts("ffffffff aaaaaaaa", [-1431655766, -1]);
	}

	private function checkSetFromBytesUnsigned(hex:String):Void {
		var t:MutableBigInt = 0;
		t.setFromBigEndianBytesUnsigned(Bytes.ofHex(hex));
		eq(BigInt.fromHexUnsigned(hex).toString(), t.toString());

		var sb = new StringBuf();
		var i = hex.length;
		while (i >= 2) {
			i -= 2;
			sb.addChar(hex.charCodeAt(i));
			sb.addChar(hex.charCodeAt(i + 1));
		}
		t.setFromLittleEndianBytesUnsigned(Bytes.ofHex(sb.toString()));
		eq(BigInt.fromHexUnsigned(hex).toString(), t.toString());
	}

	private function checkSetFromUnsignedInts(hex:String, arr:Array<Int>):Void {
		var v = Vector.fromArrayCopy(arr);
		var t:MutableBigInt = 0;
		t.setFromUnsignedInts(v, v.length);
		eq(BigInt.fromHexUnsigned(hex).toString(), t.toString());
	}

	public function bigIntCompare():Void {
		// equality, single-word
		checkCompareInt(0, 0, 0);
		checkCompareInt(0, 1, 1);
		checkCompareInt(0, -1, -1);
		checkCompareInt(0, -2147483648, -2147483648);
		checkCompareInt(0, 2147483647, 2147483647);

		// equality, multi-word
		checkCompare(0, BigInt.fromHex("12345678 9abcdef0"), BigInt.fromHex("12345678 9abcdef0"));
		checkCompare(0, BigInt.fromHex("f2345678 9abcdef0"), BigInt.fromHex("f2345678 9abcdef0"));
		checkCompare(0, BigInt.fromHex("12345678 9abcdef0 12345678"), BigInt.fromHex("12345678 9abcdef0 12345678"));
		checkCompare(0, BigInt.fromHex("f2345678 9abcdef0 12345678"), BigInt.fromHex("f2345678 9abcdef0 12345678"));

		// less than, single-word
		checkCompareInt(-1, 0, 1);
		checkCompareInt(-1, 1, 2);
		checkCompareInt(-1, -1, 0);
		checkCompareInt(-1, -2, -1);
		checkCompareInt(-1, -2147483648, 2147483647);
		checkCompareInt(-1, -2147483648, -2147483647);
		checkCompareInt(-1, -2147483648, 0);
		checkCompareInt(-1, 0, 2147483647);
		checkCompareInt(-1, 1, 2147483647);
		checkCompareInt(-1, 2147483646, 2147483647);
		checkCompareInt(-1, -1, 2147483647);

		// less than, multi-word, same length
		checkCompare(-1, BigInt.fromHex("12345678 9abcdeef"), BigInt.fromHex("12345678 9abcdef0"));
		checkCompare(-1, BigInt.fromHex("12345677 9abcdef0"), BigInt.fromHex("12345678 9abcdef0"));
		checkCompare(-1, BigInt.fromHex("f2345678 9abcdef0"), BigInt.fromHex("12345678 9abcdef0"));
		checkCompare(-1, BigInt.fromHex("f2345678 9abcdeef"), BigInt.fromHex("f2345678 9abcdef0"));
		checkCompare(-1, BigInt.fromHex("f2345677 9abcdef0"), BigInt.fromHex("f2345678 9abcdef0"));

		// less than, multi-word, different length
		checkCompare(-1, BigInt.fromHex("12345678 9abcdef0"), BigInt.fromHex("00000001 12345678 9abcdef0"));
		checkCompare(-1, BigInt.fromHex("f2345678 9abcdef0"), BigInt.fromHex("00000001 12345678 9abcdef0"));
		checkCompare(-1, BigInt.fromHex("fffffffe 12345678 9abcdef0"), BigInt.fromHex("12345678 9abcdef0"));
		checkCompare(-1, BigInt.fromHex("fffffffe 12345678 9abcdef0"), BigInt.fromHex("f2345678 9abcdef0"));

		// greater than, single-word
		checkCompareInt(1, 1, 0);
		checkCompareInt(1, 2, 1);
		checkCompareInt(1, 0, -1);
		checkCompareInt(1, -1, -2);
		checkCompareInt(1, 2147483647, 2147483646);
		checkCompareInt(1, -2147483647, -2147483648);

		// greater than, multi-word, same length
		checkCompare(1, BigInt.fromHex("12345678 9abcdef1"), BigInt.fromHex("12345678 9abcdef0"));
		checkCompare(1, BigInt.fromHex("12345679 9abcdef0"), BigInt.fromHex("12345678 9abcdef0"));
		checkCompare(1, BigInt.fromHex("12345678 9abcdef0"), BigInt.fromHex("f2345678 9abcdef0"));
		checkCompare(1, BigInt.fromHex("f2345678 9abcdef1"), BigInt.fromHex("f2345678 9abcdef0"));
		checkCompare(1, BigInt.fromHex("f2345679 9abcdef0"), BigInt.fromHex("f2345678 9abcdef0"));

		// greater than, multi-word, different length
		checkCompare(1, BigInt.fromHex("00000001 12345678 9abcdef0"), BigInt.fromHex("12345678 9abcdef0"));
		checkCompare(1, BigInt.fromHex("00000001 12345678 9abcdef0"), BigInt.fromHex("f2345678 9abcdef0"));
		checkCompare(1, BigInt.fromHex("12345678 9abcdef0"), BigInt.fromHex("fffffffe 12345678 9abcdef0"));
		checkCompare(1, BigInt.fromHex("f2345678 9abcdef0"), BigInt.fromHex("fffffffe 12345678 9abcdef0"));

		checkCompare(1, BigInt.fromHex("00000001 ffffffff"), BigInt.fromHex("00000001 00000000"));
	}

	private function checkCompareInt(expected:Int, a:Int, b:Int):Void {
		var an:BigInt = a;
		var am:MutableBigInt = a;
		switch (expected) {
			case -1:
				f(a == b);
				f(am == b);
				f(an == b);
				t(a != b);
				t(am != b);
				t(an != b);
				t(a < b);
				t(am < b);
				t(an < b);
				t(a <= b);
				t(am <= b);
				t(an <= b);
				f(a > b);
				f(am > b);
				f(an > b);
				f(a >= b);
				f(am >= b);
				f(an >= b);
			case 0:
				t(a == b);
				t(am == b);
				t(an == b);
				f(a != b);
				f(am != b);
				f(an != b);
				f(a < b);
				f(am < b);
				f(an < b);
				t(a <= b);
				t(am <= b);
				t(an <= b);
				f(a > b);
				f(am > b);
				f(an > b);
				t(a >= b);
				t(am >= b);
				t(an >= b);
			case 1:
				f(a == b);
				f(am == b);
				f(an == b);
				t(a != b);
				t(am != b);
				t(an != b);
				f(a < b);
				f(am < b);
				f(an < b);
				f(a <= b);
				f(am <= b);
				f(an <= b);
				t(a > b);
				t(am > b);
				t(an > b);
				t(a >= b);
				t(am >= b);
				t(an >= b);
		}
		checkCompare(expected, BigInt.fromInt(a), BigInt.fromInt(b));
	}

	private function checkCompare(expected:Int, a:BigInt, b:BigInt):Void {
		checkCompareSingle(expected, a, b);
		checkCompareSingle(-expected, -a, -b);
		if ((expected != 0) && (a.sign() == b.sign())) {
			var s:Int = (a.sign() << 1) + 1;
			checkCompareSingle(-s, -a, b);
			checkCompareSingle(s, a, -b);
		}
	}

	private function checkCompareSingle(expected:Int, a:BigInt, b:BigInt):Void {
		eq(expected, BigIntArithmetic.compare(a, b));
		if (expected == 0) {
			eq(expected, BigIntArithmetic.compare(b, a));
		} else {
			eq(-expected, BigIntArithmetic.compare(b, a));
		}

		var am:MutableBigInt = a;
		var bm:MutableBigInt = b;
		switch (expected) {
			case -1:
				f(a == b);
				f(am == b);
				f(a == bm);
				f(am == bm);
				t(a != b);
				t(am != b);
				t(a != bm);
				t(am != bm);
				t(a < b);
				t(am < b);
				t(a < bm);
				t(am < bm);
				t(a <= b);
				t(am <= b);
				t(a <= bm);
				t(am <= bm);
				f(a > b);
				f(am > b);
				f(a > bm);
				f(am > bm);
				f(a >= b);
				f(am >= b);
				f(a >= bm);
				f(am >= bm);
			case 0:
				t(a == b);
				t(am == b);
				t(a == bm);
				t(am == bm);
				f(a != b);
				f(am != b);
				f(a != bm);
				f(am != bm);
				f(a < b);
				f(am < b);
				f(a < bm);
				f(am < bm);
				t(a <= b);
				t(am <= b);
				t(a <= bm);
				t(am <= bm);
				f(a > b);
				f(am > b);
				f(a > bm);
				f(am > bm);
				t(a >= b);
				t(am >= b);
				t(a >= bm);
				t(am >= bm);
			case 1:
				f(a == b);
				f(am == b);
				f(a == bm);
				f(am == bm);
				t(a != b);
				t(am != b);
				t(a != bm);
				t(am != bm);
				f(a < b);
				f(am < b);
				f(a < bm);
				f(am < bm);
				f(a <= b);
				f(am <= b);
				f(a <= bm);
				f(am <= bm);
				t(a > b);
				t(am > b);
				t(a > bm);
				t(am > bm);
				t(a >= b);
				t(am >= b);
				t(a >= bm);
				t(am >= bm);
		}
	}

	public function bigIntMultiplicationAndDivisionSemantics():Void {
		var dividend:MutableBigInt = 0;
		var divisor:MutableBigInt;
		var quotient:MutableBigInt;
		var remainder:MutableBigInt = 0;
		var remainderInt:Int;

		// if result of multiplication is an input, should throw an exception
		var a:MutableBigInt = 2;
		try {
			BigIntArithmetic.multiply(a, a, BigInt.fromInt(2));
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			BigIntArithmetic.multiply(a, BigInt.fromInt(2), a);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		// division by zero should throw exception
		try {
			BigInt.fromInt(1) / 0;
		} catch (e) {
			eq(cast BigIntExceptions.DIVISION_BY_ZERO, e.message);
		}
		try {
			BigInt.fromInt(0) / 0;
		} catch (e) {
			eq(cast BigIntExceptions.DIVISION_BY_ZERO, e.message);
		}

		// Multiplication of same inputs is ok
		var b:MutableBigInt = 0;
		BigIntArithmetic.multiply(b, a, a);
		eq("4", b.toString());

		// check quotient overlaps with dividend, divisor < 65536 case
		quotient = 12345;
		remainderInt = BigIntArithmetic.divideInt(quotient, 11, quotient);
		eq("1122", quotient.toString());
		eq(3, remainderInt);
		quotient = 12345;
		BigIntArithmetic.divide(quotient, BigInt.fromInt(11), quotient, remainder);
		eq("1122", quotient.toString());
		eq("3", remainder.toString());

		// check quotient overlaps with dividend, divisor >= 65536 case
		quotient = 721018;
		divisor = 65537;
		BigIntArithmetic.divide(quotient, divisor, quotient, remainder);
		eq("11", quotient.toString());
		eq("111", remainder.toString());

		// check quotient overlaps with dividend, special case 1
		quotient = 0;
		divisor = 11;
		BigIntArithmetic.divide(quotient, divisor, quotient, remainder);
		eq("0", quotient.toString());
		eq("0", remainder.toString());

		// check quotient overlaps with dividend, special case 2
		quotient = 7;
		BigIntArithmetic.divide(quotient, BigInt.fromInt(1), quotient, remainder);
		eq("7", quotient.toString());
		eq("0", remainder.toString());

		// check quotient overlaps with dividend, special case 3
		quotient = 11;
		BigIntArithmetic.divide(quotient, BigInt.fromHex("1 00000000"), quotient, remainder);
		eq("0", quotient.toString());
		eq("11", remainder.toString());

		// check quotient overlaps with divisor, divisor < 65536 case
		quotient = 11;
		BigIntArithmetic.divide(BigInt.fromInt(12345), quotient, quotient, remainder);
		eq("1122", quotient.toString());
		eq("3", remainder.toString());

		// check quotient overlaps with divisor, divisor >= 65536 case
		quotient = 65537;
		BigIntArithmetic.divide(BigInt.fromInt(721018), quotient, quotient, remainder);
		eq("11", quotient.toString());
		eq("111", remainder.toString());

		// check quotient overlaps with divisor, special case 1
		quotient = 10;
		BigIntArithmetic.divide(BigInt.fromInt(0), quotient, quotient, remainder);
		eq("0", quotient.toString());
		eq("0", remainder.toString());

		// check quotient overlaps with divisor, special case 2
		quotient = 1;
		BigIntArithmetic.divide(BigInt.fromInt(7), quotient, quotient, remainder);
		eq("7", quotient.toString());
		eq("0", remainder.toString());

		// check quotient overlaps with divisor, special case 3
		quotient = BigInt.fromHex("1 00000000");
		BigIntArithmetic.divide(BigInt.fromInt(11), quotient, quotient, remainder);
		eq("0", quotient.toString());
		eq("11", remainder.toString());

		// check remainder overlaps with dividend, divisor < 65536 case
		remainder = 12345;
		BigIntArithmetic.divide(remainder, BigInt.fromInt(11), quotient, remainder);
		eq("1122", quotient.toString());
		eq("3", remainder.toString());

		// check remainder overlaps with dividend, divisor >= 65536 case
		remainder = 721018;
		BigIntArithmetic.divide(remainder, BigInt.fromInt(65537), quotient, remainder);
		eq("11", quotient.toString());
		eq("111", remainder.toString());

		// check remainder overlaps with dividend, special case 1
		remainder = 0;
		BigIntArithmetic.divide(remainder, BigInt.fromInt(10), quotient, remainder);
		eq("0", quotient.toString());
		eq("0", remainder.toString());

		// check remainder overlaps with dividend, special case 2
		remainder = 7;
		BigIntArithmetic.divide(remainder, BigInt.fromInt(1), quotient, remainder);
		eq("7", quotient.toString());
		eq("0", remainder.toString());

		// check remainder overlaps with dividend, special case 3
		remainder = 11;
		BigIntArithmetic.divide(remainder, BigInt.fromHex("1 00000000"), quotient, remainder);
		eq("0", quotient.toString());
		eq("11", remainder.toString());

		// check remainder overlaps with divisor, divisor < 65536 case
		remainder = 11;
		BigIntArithmetic.divide(BigInt.fromInt(12345), remainder, quotient, remainder);
		eq("1122", quotient.toString());
		eq("3", remainder.toString());

		// check remainder overlaps with divisor, divisor >= 65536 case
		remainder = 65537;
		BigIntArithmetic.divide(BigInt.fromInt(721018), remainder, quotient, remainder);
		eq("11", quotient.toString());
		eq("111", remainder.toString());

		// check remainder overlaps with divisor, special case 1
		remainder = 10;
		BigIntArithmetic.divide(BigInt.fromInt(0), remainder, quotient, remainder);
		eq("0", quotient.toString());
		eq("0", remainder.toString());

		// check remainder overlaps with divisor, special case 2
		remainder = 1;
		BigIntArithmetic.divide(BigInt.fromInt(7), remainder, quotient, remainder);
		eq("7", quotient.toString());
		eq("0", remainder.toString());

		// check remainder overlaps with divisor, special case 3
		remainder = BigInt.fromHex("1 00000000");
		BigIntArithmetic.divide(BigInt.fromInt(11), remainder, quotient, remainder);
		eq("0", quotient.toString());
		eq("11", remainder.toString());

		// quotient and remainder cannot be the same object
		try {
			BigIntArithmetic.divide(BigInt.fromInt(1), BigInt.fromInt(10), quotient, quotient);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// quotient cannot be null
		try {
			BigIntArithmetic.divideInt(BigInt.fromInt(1), 10, null);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			BigIntArithmetic.divide(BigInt.fromInt(1), BigInt.fromInt(10), null, remainder);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// remainder can be null
		BigIntArithmetic.divide(BigInt.fromInt(1), BigInt.fromInt(10), quotient, null);
		eq("0", quotient.toString());

		// dividend and divisor can be the same object
		divisor = 10;
		BigIntArithmetic.divide(divisor, divisor, quotient, remainder);
		eq("1", quotient.toString());
		eq("0", remainder.toString());

		// work may not overlap any input
		try {
			BigIntArithmetic.divide(dividend, divisor, quotient, remainder, dividend);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			BigIntArithmetic.divide(dividend, divisor, quotient, remainder, divisor);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			BigIntArithmetic.divide(dividend, divisor, quotient, remainder, quotient);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			BigIntArithmetic.divide(dividend, divisor, quotient, remainder, remainder);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
	}

	public function bigIntMultiplicationAndDivision():Void {
		checkLinearEqInt(BigInt.fromInt(0), 0, BigInt.fromInt(0), 0);
		checkLinearEqInt(BigInt.fromInt(0), 1, BigInt.fromInt(0), 0);
		checkLinearEqInt(BigInt.fromInt(0), 0, BigInt.fromInt(1), 0);
		checkLinearEqInt(BigInt.fromInt(0), 100, BigInt.fromInt(0), 0);
		checkLinearEqInt(BigInt.fromInt(1), 1, BigInt.fromInt(1), 0);
		checkLinearEqInt(BigInt.fromInt(1), 2, BigInt.fromInt(0), 1);
		checkLinearEqInt(BigInt.fromInt(2), 1, BigInt.fromInt(2), 0);
		checkLinearEqInt(BigInt.fromInt(2), 3, BigInt.fromInt(0), 2);
		checkLinearEqInt(BigInt.fromInt(3), 3, BigInt.fromInt(1), 0);
		checkLinearEqInt(BigInt.fromInt(4), 2, BigInt.fromInt(2), 0);
		checkLinearEqInt(BigInt.fromInt(4), 3, BigInt.fromInt(1), 1);
		checkLinearEqInt(BigInt.fromInt(5), 3, BigInt.fromInt(1), 2);
		checkLinearEqInt(BigInt.fromInt(6), 3, BigInt.fromInt(2), 0);
		checkLinearEqInt(BigInt.fromInt(6), 2, BigInt.fromInt(3), 0);
		checkLinearEqInt(BigInt.fromHex("12A05F2001"), 81, BigInt.fromInt(987654321), 0);

		checkLinearEq(BigInt.fromHex("0 fffffffe 00000001"), BigInt.fromHex("0 ffffffff"), BigInt.fromHex("0 ffffffff"),
			BigInt.fromInt(0)); // exercises qhat = 65536
		checkLinearEq(BigInt.fromHex("00003fff c0000000 7fff8000 00000000"), BigInt.fromHex("7fff8000 00000000"), BigInt.fromHex("00008000 00000001"),
			BigInt.fromInt(0));

		checkLinearEqInt(BigInt.fromInt(2147483647), 1, BigInt.fromInt(2147483647), 0);
		checkLinearEqInt(BigInt.fromInt(2147483647), 10, BigInt.fromInt(214748364), 7);
		checkLinearEqInt(BigInt.fromInt(2147483647), 100, BigInt.fromInt(21474836), 47);
		checkLinearEqInt(BigInt.fromInt(2147483647), 1000, BigInt.fromInt(2147483), 647);
		checkLinearEqInt(BigInt.fromInt(2147483647), 10000, BigInt.fromInt(214748), 3647);
		checkLinearEqInt(BigInt.fromInt(2147483647), 100000, BigInt.fromInt(21474), 83647); // exercises rhat >= 65536
		checkLinearEqInt(BigInt.fromInt(2147483647), 1000000, BigInt.fromInt(2147), 483647);
		checkLinearEqInt(BigInt.fromInt(2147483647), 10000000, BigInt.fromInt(214), 7483647);
		checkLinearEqInt(BigInt.fromInt(2147483647), 100000000, BigInt.fromInt(21), 47483647);
		checkLinearEqInt(BigInt.fromInt(2147483647), 1000000000, BigInt.fromInt(2), 147483647);

		checkLinearEqInt(BigInt.fromInt(2147483647), 2147483647, BigInt.fromInt(1), 0); // exercises use of uninitialized quotient data

		checkLinearEqInt(BigInt.fromHex("100000000"), 1, BigInt.fromHex("100000000"), 0);
		checkLinearEqInt(BigInt.fromHex("100000000"), 10, BigInt.fromInt(429496729), 6);
		checkLinearEqInt(BigInt.fromHex("100000000"), 100, BigInt.fromInt(42949672), 96);
		checkLinearEqInt(BigInt.fromHex("100000000"), 1000, BigInt.fromInt(4294967), 296);
		checkLinearEqInt(BigInt.fromHex("100000000"), 10000, BigInt.fromInt(429496), 7296);
		checkLinearEqInt(BigInt.fromHex("100000000"), 100000, BigInt.fromInt(42949), 67296); // exercises rhat >= 65536
		checkLinearEqInt(BigInt.fromHex("100000000"), 1000000, BigInt.fromInt(4294), 967296);
		checkLinearEqInt(BigInt.fromHex("100000000"), 10000000, BigInt.fromInt(429), 4967296);
		checkLinearEqInt(BigInt.fromHex("100000000"), 100000000, BigInt.fromInt(42), 94967296);
		checkLinearEqInt(BigInt.fromHex("100000000"), 1000000000, BigInt.fromInt(4), 294967296);
		checkLinearEq(BigInt.fromHex("100000000"), BigInt.fromHex("2540BE400"), BigInt.fromInt(0), BigInt.fromHex("100000000"));

		checkLinearEqInt(BigInt.fromHex("08000"), 1, BigInt.fromHex("08000"), 0);
		checkLinearEqInt(BigInt.fromHex("080000000"), 1, BigInt.fromHex("080000000"), 0);
		checkLinearEqInt(BigInt.fromHex("0800000000000"), 1, BigInt.fromHex("0800000000000"), 0);
		checkLinearEqInt(BigInt.fromHex("08000000000000000"), 1, BigInt.fromHex("08000000000000000"), 0);
		checkLinearEqInt(BigInt.fromHex("10001"), 2, BigInt.fromHex("08000"), 1);
		checkLinearEqInt(BigInt.fromHex("100000001"), 2, BigInt.fromHex("080000000"), 1);
		checkLinearEqInt(BigInt.fromHex("1000000000001"), 2, BigInt.fromHex("0800000000000"), 1);
		checkLinearEqInt(BigInt.fromHex("10000000000000001"), 2, BigInt.fromHex("08000000000000000"), 1);

		checkLinearEqInt(BigInt.fromHex("0ffffffff"), 1, BigInt.fromHex("0ffffffff"), 0);
		checkLinearEqInt(BigInt.fromHex("0ffffffffffffffff"), 1, BigInt.fromHex("0ffffffffffffffff"), 0);
		checkLinearEqInt(BigInt.fromHex("0ffffffffffffffffffffffff"), 1, BigInt.fromHex("0ffffffffffffffffffffffff"), 0);
		checkLinearEqInt(BigInt.fromHex("0ffffffff"), 2, BigInt.fromHex("07fffffff"), 1);
		checkLinearEqInt(BigInt.fromHex("0ffffffffffffffff"), 2, BigInt.fromHex("07fffffffffffffff"), 1);
		checkLinearEqInt(BigInt.fromHex("0ffffffffffffffffffffffff"), 2, BigInt.fromHex("07fffffffffffffffffffffff"), 1);

		// exercise quotient with high bit set when length of divisor == length of dividend and divisor >= 65536
		checkLinearEq(BigInt.fromHex("4000000000000000"), BigInt.fromHex("080000000"), BigInt.fromHex("080000000"),
			BigInt.fromInt(0)); // exercises uninitialized work data
		checkLinearEq(BigInt.fromHex("4000000080000000"), BigInt.fromHex("080000001"), BigInt.fromHex("080000000"), BigInt.fromInt(0));
		checkLinearEq(BigInt.fromHex("4000000100000000"), BigInt.fromHex("080000001"), BigInt.fromHex("080000000"), BigInt.fromHex("080000000"));
		checkLinearEq(BigInt.fromHex("40000000ffffffff"), BigInt.fromHex("080000001"), BigInt.fromHex("080000000"), BigInt.fromHex("7fffffff"));
		checkLinearEq(BigInt.fromHex("4000000100000001"), BigInt.fromHex("080000001"), BigInt.fromHex("080000001"), BigInt.fromInt(0));

		checkLinearEq(BigInt.fromHex("08000"), BigInt.fromHex("0800000001"), BigInt.fromHex("0"), BigInt.fromHex("08000"));
		// these exercise the qhat reduction path
		checkLinearEq(BigInt.fromHex("080000000"), BigInt.fromHex("080000001"), BigInt.fromHex("0"), BigInt.fromHex("080000000"));
		checkLinearEq(BigInt.fromHex("0800080010000"), BigInt.fromHex("080000001"), BigInt.fromHex("10000"), BigInt.fromHex("080000000"));
		checkLinearEq(BigInt.fromHex("0800100010001"), BigInt.fromHex("080000001"), BigInt.fromHex("10001"), BigInt.fromHex("080000000"));
		checkLinearEq(BigInt.fromHex("08000000180000000"), BigInt.fromHex("080000001"), BigInt.fromHex("100000000"), BigInt.fromHex("080000000"));
		checkLinearEq(BigInt.fromHex("08000000200000001"), BigInt.fromHex("080000001"), BigInt.fromHex("100000001"), BigInt.fromHex("080000000"));

		// this exercises long division with a quotient with high bit set
		checkLinearEq(BigInt.fromHex("08000000180000000"), BigInt.fromHex("100000000"), BigInt.fromHex("080000001"), BigInt.fromHex("080000000"));

		// these exercise the "add back" path
		checkLinearEq(BigInt.fromHex("7fff800000000000"), BigInt.fromHex("0800000000001"), BigInt.fromHex("0fffe"), BigInt.fromHex("7fffffff0002"));
		checkLinearEq(BigInt.fromHex("7fffffff800000010000000000000000"), BigInt.fromHex("0800000008000000200000005"), BigInt.fromHex("0fffffffd"),
			BigInt.fromHex("080000000800000010000000f"));

		checkLinearEq(BigInt.fromInt(1), BigInt.fromHex("100000000"), BigInt.fromInt(0), BigInt.fromInt(1));
	}

	private function checkLinearEqInt(y:BigInt, a:Int, x:BigInt, b:Int):Void {
		// checks that y = ax + b
		eq(y.toHex(), (a * x + b).toHex());
		eq(y.toHex(), (x * a + b).toHex());
		eq(y.toHex(), (b + a * x).toHex());
		eq(y.toHex(), (b + x * a).toHex());
		checkMultiplyInt(x, a, y - b);
		if (a != 0) {
			checkDivInt(y, a, x, b);
		}
		checkLinearEq(y, BigInt.fromInt(a), x, BigInt.fromInt(b));
	}

	private function checkLinearEq(y:BigInt, a:BigInt, x:BigInt, b:BigInt):Void {
		// checks that y = ax + b
		eq(y.toHex(), (a * x + b).toHex());
		eq(y.toHex(), (x * a + b).toHex());
		eq(y.toHex(), (b + a * x).toHex());
		eq(y.toHex(), (b + x * a).toHex());
		checkMultiply(a, x, y - b);
		if (!a.isZero()) {
			checkDiv(y, a, x, b);
		}
		// if we have n / d = q + r / d, then n / q = n * d / (n - r)
		var y_b = y - b;
		if (!y_b.isZero()) {
			var q2 = y * a / y_b;
			var r2 = y - q2 * x;
			checkDiv(y, x, q2, r2);
		}
	}

	private function checkMultiplyInt(a:BigInt, b:Int, expected:BigInt):Void {
		var am:MutableBigInt = a;
		eq(expected.toHex(), (a * b).toHex());
		eq(expected.toHex(), (am * b).toHex());
		am *= b;
		eq(expected.toHex(), am.toHex());

		checkMultiply(a, BigInt.fromInt(b), expected);
	}

	private function checkMultiply(a:BigInt, b:BigInt, expected:BigInt):Void {
		checkMultiplyCommute(a, b, expected);
		checkMultiplyCommute(-a, b, -expected);
		checkMultiplyCommute(a, -b, -expected);
		checkMultiplyCommute(-a, -b, expected);
	}

	private function checkMultiplyCommute(a:BigInt, b:BigInt, expected:BigInt):Void {
		checkMultiplySingle(a, b, expected);
		checkMultiplySingle(b, a, expected);
	}

	private function checkMultiplySingle(a:BigInt, b:BigInt, expected:BigInt):Void {
		var am:MutableBigInt = a;
		var bm:MutableBigInt = b;

		eq(expected.toString(), (a * b).toString());
		eq(expected.toString(), (am * b).toString());
		eq(expected.toString(), (a * bm).toString());
		eq(expected.toString(), (am * bm).toString());

		am = a;
		am *= b;
		eq(expected.toString(), am.toString());
		am = a;
		am *= bm;
		eq(expected.toString(), am.toString());
	}

	private function checkDivInt(dividend:BigInt, divisor:Int, expectedQuotient:BigInt, expectedRemainder:Int):Void {
		var dividendM:MutableBigInt = dividend;

		var quotient:MutableBigInt = 0;
		var remainder:Int;
		remainder = BigIntArithmetic.divideInt(dividend, divisor, quotient);
		eq(expectedRemainder, remainder);
		eq(expectedQuotient.toString(), quotient.toString());

		eq(expectedQuotient.toString(), (dividend / divisor).toString());
		eq(expectedQuotient.toString(), (dividendM / divisor).toString());
		eq(expectedRemainder, (dividend % divisor));
		eq(expectedRemainder, (dividendM % divisor));

		dividendM = dividend;
		dividendM /= divisor;
		eq(expectedQuotient.toString(), dividendM.toString());
		dividendM = dividend;
		dividendM %= divisor;
		eq(Std.string(expectedRemainder), dividendM.toString());

		checkDiv(dividend, BigInt.fromInt(divisor), expectedQuotient, BigInt.fromInt(expectedRemainder));
	}

	private function checkDiv(dividend:BigInt, divisor:BigInt, expectedQuotient:BigInt, expectedRemainder:BigInt):Void {
		checkDivSingle(dividend, divisor, expectedQuotient, expectedRemainder);
		checkDivSingle(dividend, -divisor, -expectedQuotient, expectedRemainder);
		checkDivSingle(-dividend, divisor, -expectedQuotient, -expectedRemainder);
		checkDivSingle(-dividend, -divisor, expectedQuotient, -expectedRemainder);
	}

	private function checkDivSingle(dividend:BigInt, divisor:BigInt, expectedQuotient:BigInt, expectedRemainder:BigInt):Void {
		var dividendM:MutableBigInt = dividend;
		var divisorM:MutableBigInt = divisor;

		var quotient:MutableBigInt = 0;
		var remainder:MutableBigInt = 0;
		BigIntArithmetic.divide(dividend, divisor, quotient, remainder);
		eq(expectedRemainder.toString(), remainder.toString());
		eq(expectedQuotient.toString(), quotient.toString());

		eq(dividend.toString(), (quotient * divisor + remainder).toString());

		eq(expectedQuotient.toString(), (dividend / divisor).toString());
		eq(expectedQuotient.toString(), (dividendM / divisor).toString());
		eq(expectedQuotient.toString(), (dividend / divisorM).toString());
		eq(expectedQuotient.toString(), (dividendM / divisorM).toString());
		eq(expectedRemainder.toString(), (dividend % divisor).toString());
		eq(expectedRemainder.toString(), (dividendM % divisor).toString());
		eq(expectedRemainder.toString(), (dividend % divisorM).toString());
		eq(expectedRemainder.toString(), (dividendM % divisorM).toString());

		dividendM = dividend;
		dividendM /= divisor;
		eq(expectedQuotient.toString(), dividendM.toString());
		dividendM = dividend;
		dividendM /= divisorM;
		eq(expectedQuotient.toString(), dividendM.toString());
		dividendM = dividend;
		dividendM %= divisor;
		eq(expectedRemainder.toString(), dividendM.toString());
		dividendM = dividend;
		dividendM %= divisorM;
		eq(expectedRemainder.toString(), dividendM.toString());
	}

	public function bigIntEquality():Void {
		checkEquality(BigInt.fromInt(0), BigInt.fromInt(0), true);
		checkEquality(BigInt.fromInt(0), BigInt.fromInt(1), false);
		checkEquality(BigInt.fromInt(1), BigInt.fromInt(1), true);
		checkEquality(BigInt.fromInt(0x12345678), BigInt.fromInt(0x12345678), true);
		checkEquality(BigInt.fromInt(0x12345678), BigInt.fromInt(0x12345670), false);
		checkEquality(BigInt.fromHex("1234567800000000"), BigInt.fromInt(0), false);
		checkEquality(BigInt.fromHex("1234567800000000"), BigInt.fromHex("1234567800000000"), true);
		checkEquality(BigInt.fromHex("1234567800000000"), BigInt.fromHex("11234567800000000"), false);
		checkEquality(BigInt.fromHex("1234567800000000"), BigInt.fromHex("123456780000000"), false);
	}

	private function checkEquality(a:BigInt, b:BigInt, expected:Bool):Void {
		eq(expected, a == b);
		eq(expected, b == a);

		var a1:MutableBigInt = a;
		eq(expected, a1 == b);
		eq(expected, b == a1);

		var b1:MutableBigInt = b;
		eq(expected, a == b1);
		eq(expected, b1 == a);

		eq(expected, a1 == b1);
		eq(expected, b1 == a1);
	}

	public function bigIntAdditionAndSubtraction():Void {
		checkAddInt(BigInt.fromInt(0), 0, BigInt.fromInt(0));
		checkAddInt(BigInt.fromInt(0), 1, BigInt.fromInt(1));
		checkAddInt(BigInt.fromInt(1), 1, BigInt.fromInt(2));
		checkAddInt(BigInt.fromInt(-1), 0, BigInt.fromInt(-1));
		checkAddInt(BigInt.fromInt(-1), 1, BigInt.fromInt(0));
		checkAddInt(BigInt.fromInt(-1), 2, BigInt.fromInt(1));
		checkAddInt(BigInt.fromInt(-1), -1, BigInt.fromInt(-2));

		checkAddInt(BigInt.fromHex("000000000000000007fffffff"), 1, BigInt.fromHex("0000000000000000080000000"));
		checkAddInt(BigInt.fromHex("0000000007fffffffffffffff"), 1, BigInt.fromHex("0000000008000000000000000"));
		checkAddInt(BigInt.fromHex("07fffffffffffffffffffffff"), 1, BigInt.fromHex("0800000000000000000000000"));
		checkAddInt(BigInt.fromHex("0ffffffffffffffffffffffff"), 1, BigInt.fromHex("1000000000000000000000000"));
		checkAddInt(BigInt.fromHex("0fffffffffffffffeffffffff"), 1, BigInt.fromHex("0ffffffffffffffff00000000"));

		checkAdd(BigInt.fromHex("0ffffffffffffffff00000000"), BigInt.fromHex("100000000"), BigInt.fromHex("1000000000000000000000000"));
		checkAdd(BigInt.fromHex("0ffffffff0000000000000000"), BigInt.fromHex("10000000000000000"), BigInt.fromHex("1000000000000000000000000"));

		checkAdd(BigInt.fromHex("12345678"), BigInt.fromHex("11111111"), BigInt.fromHex("23456789"));
		checkAdd(BigInt.fromHex("1234567812345678"), BigInt.fromHex("1111111111111111"), BigInt.fromHex("2345678923456789"));
		checkAdd(BigInt.fromHex("123456781234567812345678"), BigInt.fromHex("111111111111111111111111"), BigInt.fromHex("234567892345678923456789"));
		checkAdd(BigInt.fromHex("1234567812345678"), BigInt.fromHex("11111111"), BigInt.fromHex("1234567823456789"));
		checkAdd(BigInt.fromHex("123456781234567812345678"), BigInt.fromHex("11111111"), BigInt.fromHex("123456781234567823456789"));
		checkAdd(BigInt.fromHex("1234567812345678"), BigInt.fromHex("1111111100000000"), BigInt.fromHex("2345678912345678"));
		checkAdd(BigInt.fromHex("123456781234567812345678"), BigInt.fromHex("111111110000000000000000"), BigInt.fromHex("234567891234567812345678"));
		checkAdd(BigInt.fromHex("123456781234567812345678"), BigInt.fromHex("111111110000000011111111"), BigInt.fromHex("234567891234567823456789"));
	}

	private function checkAddInt(a:BigInt, b:Int, expected:BigInt):Void {
		var am:MutableBigInt = a;
		eq(expected.toString(), (a + b).toString());
		eq(expected.toString(), (am + b).toString());
		am += b;
		eq(expected.toString(), am.toString());

		checkAdd(a, BigInt.fromInt(b), expected);
	}

	private function checkAdd(a:BigInt, b:BigInt, expected:BigInt):Void {
		checkAddCommute(a, b, expected);
		checkAddCommute(-a, -b, -expected);
		checkAddCommute(expected, -a, b);
		checkAddCommute(expected, -b, a);
	}

	private function checkAddCommute(a:BigInt, b:BigInt, expected:BigInt):Void {
		checkAddSingle(a, b, expected);
		checkAddSingle(b, a, expected);
	}

	private function checkAddSingle(a:BigInt, b:BigInt, expected:BigInt):Void {
		var am:MutableBigInt = a;
		var bm:MutableBigInt = b;
		var em:MutableBigInt = expected;

		// addition
		eq(expected.toString(), (a + b).toString());
		eq(expected.toString(), (am + bm).toString());
		eq(expected.toString(), (am + b).toString());
		eq(expected.toString(), (a + bm).toString());
		am = a;
		am += b;
		eq(expected.toString(), am.toString());
		am = a;
		am += bm;
		eq(expected.toString(), am.toString());

		// subtraction
		eq(a.toString(), (expected - b).toString());
		eq(a.toString(), (expected - bm).toString());
		eq(a.toString(), (em - b).toString());
		eq(a.toString(), (em - bm).toString());
		em = expected;
		em -= b;
		eq(a.toString(), em.toString());
		em = expected;
		em -= bm;
		eq(a.toString(), em.toString());
	}

	public function bigIntAddAssignDoesntClobber():Void {
		var a:BigInt = BigInt.fromInt(1);
		var b:MutableBigInt = a;
		b += 1;
		eq("1", a.toString());
		eq("2", b.toString());
	}

	public function bigIntNegate():Void {
		eq("0", (-BigInt.fromInt(0)).toString());
		eq(BigInt.fromInt(-1).toString(), (-BigInt.fromInt(1)).toString());
		eq("1", (-BigInt.fromInt(-1)).toString());
		eq(BigInt.fromInt(-100).toString(), (-BigInt.fromInt(100)).toString());
		eq("100", (-BigInt.fromInt(-100)).toString());
		eq(BigInt.fromHex("080000000").toHex(), (-BigInt.fromInt(-2147483648)).toHex());
		eq(BigInt.fromInt(-2147483648).toHex(), (-BigInt.fromHex("080000000")).toHex());
		eq(BigInt.fromHex("08000000000000000").toHex(), (-BigInt.fromHex("8000000000000000")).toHex());
		eq(BigInt.fromHex("8000000000000000").toHex(), (-BigInt.fromHex("08000000000000000")).toHex());
		eq(BigInt.fromHex("edcba98800000000").toHex(), (-BigInt.fromHex("1234567800000000")).toHex());
	}

	public function bigIntFromString():Void {
		try {
			var x = BigInt.fromString(null);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			var x = BigInt.fromString("");
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			var x = BigInt.fromString("-");
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			var x = BigInt.fromString(" 0");
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			var x = BigInt.fromString("0 ");
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		eq("0", BigInt.fromString("0").toString());
		eq("1", BigInt.fromString("1").toString());
		eq("ffffffff", BigInt.fromString("-1").toHex());
		eq("100", BigInt.fromString("100").toString());
		eq(BigInt.fromInt(-100).toHex(), BigInt.fromString("-100").toHex());
		eq("7fffffff", BigInt.fromString("2147483647").toHex());
		eq("7fffffff", BigInt.fromString("02147483647").toHex());
		eq(BigInt.fromInt(-2147483648).toHex(), BigInt.fromString("-2147483648").toHex());
		eq(BigInt.fromInt(-2147483648).toHex(), BigInt.fromString("-02147483648").toHex());
		eq(BigInt.fromHex("080000000").toHex(), BigInt.fromString("2147483648").toHex());
		eq(BigInt.fromHex("f7fffffff").toHex(), BigInt.fromString("-2147483649").toHex());

		var a:MutableBigInt = 1;
		for (i in 0...96) {
			eq(a.toString(), BigInt.fromString(s_powersOfTwo[i]).toString());
			eq((-a).toString(), BigInt.fromString("-" + s_powersOfTwo[i]).toString());
			a <<= 1;
		}
	}
	
	public function bigIntFromBytes():Void 
	{
		try {
			var x = BigInt.fromBytes(null);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		eq("0", BigInt.fromBytes(Bytes.ofHex("00")).toString());
		eq("1", BigInt.fromBytes(Bytes.ofHex("01")).toString());
		eq("ffffffff", BigInt.fromBytes(Bytes.ofHex("ffffffff")).toHex());
		eq("100", BigInt.fromBytes(Bytes.ofHex("64")).toString());
		eq(BigInt.fromInt(-100).toHex(), BigInt.fromBytes(Bytes.ofHex("ffffff9c")).toHex());
		eq("7fffffff", BigInt.fromBytes(Bytes.ofHex("7fffffff")).toHex());
		eq(BigInt.fromInt(-2147483648).toHex(), BigInt.fromBytes(Bytes.ofHex("80000000")).toHex());
		eq(BigInt.fromHex("f7fffffff").toHex(), BigInt.fromBytes(Bytes.ofHex("ffffffff7fffffff")).toHex());
	}

	public function bigIntArithmeticShiftLeftAssignDoesntClobber():Void {
		var a:BigInt = BigInt.fromInt(1);
		var b:MutableBigInt = a;
		b <<= 1;
		eq("1", a.toString());
		eq("2", b.toString());
	}

	public function bigIntArithmeticShiftRightAssignDoesntClobber():Void {
		var a:BigInt = BigInt.fromInt(2);
		var b:MutableBigInt = a;
		b >>= 1;
		eq("2", a.toString());
		eq("1", b.toString());
	}

	public function bigIntArithmeticShiftLeft():Void {
		asl(BigInt.ZERO, 0, BigInt.ZERO);
		asl(BigInt.ZERO, 1, BigInt.ZERO);
		asl(BigInt.ZERO, 128, BigInt.ZERO);
		asl(BigInt.ZERO, 2147483647, BigInt.ZERO);

		var a:MutableBigInt = BigInt.ZERO;
		try {
			a <<= -1;
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			a << -1;
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			BigInt.ONE << -1;
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		asl(BigInt.ONE, 1, BigInt.fromInt(2));
		asl(BigInt.ONE, 31, BigInt.fromHex("080000000"));
		asl(BigInt.fromHex("080000000"), 1, BigInt.fromHex("100000000"));

		var sb = new StringBuf();
		sb.add("1");
		for (i in 0...100) {
			asl(BigInt.ONE, i * 4, BigInt.fromHex(sb.toString()));
			sb.add("0");
		}
		sb = new StringBuf();
		sb.add("08");
		for (i in 0...100) {
			asl(BigInt.ONE, i * 4 + 3, BigInt.fromHex(sb.toString()));
			sb.add("0");
		}

		asl(BigInt.fromHex("08000000180000000"), 15, BigInt.fromHex("40000000c00000000000"));
	}

	private function asl(a:BigInt, b:Int, expected:BigInt):Void {
		var result:MutableBigInt = 0;
		BigIntArithmetic.arithmeticShiftLeft(result, a, b);
		eq(expected.toHex(), result.toHex());
		eq(expected.toHex(), (a << b).toHex());
		result = a;
		result <<= b;
		eq(expected.toHex(), result.toHex());

		BigIntArithmetic.arithmeticShiftRight(result, expected, b);
		eq(a.toHex(), result.toHex());
		eq(a.toHex(), (expected >> b).toHex());
		result = expected;
		result >>= b;
		eq(a.toHex(), result.toHex());
	}

	public function bigIntIsZero():Void {
		t(BigInt.ZERO.isZero());
		f(BigInt.ONE.isZero());
		f(BigInt.NEGATIVE_ONE.isZero());
	}

	public function bigIntSign():Void {
		eq(0, BigInt.ZERO.sign());
		eq(0, BigInt.ONE.sign());
		eq(-1, BigInt.NEGATIVE_ONE.sign());
		eq(0, BigInt.fromInt(2147483647).sign());
		eq(-1, BigInt.fromInt(-2147483648).sign());
	}

	public function bigIntSetFromIntWithLargeLength():Void {
		var x:MutableBigInt = BigInt.fromHex("1 00000000");
		x <<= 1; // make it owned
		var y:MutableBigInt_ = x;
		y.setFromInt(11);
		eq("11", y.toString());
	}

	public function bigIntHexStrings():Void {
		eq("00000000", BigInt.ZERO.toHex());
		eq("00000001", BigInt.ONE.toHex());
		eq("ffffffff", BigInt.NEGATIVE_ONE.toHex());

		eq("0", BigInt.fromHex("0").toString());
		eq("1", BigInt.fromHex("1").toString());
		eq(BigInt.fromInt(-1).toHex(), BigInt.fromHex("f").toHex());

		try {
			var x = BigInt.fromHex(null);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			var x = BigInt.fromHex("");
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			var x = BigInt.fromHex("0q0");
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		var sb = new StringBuf();
		var v = 1;
		for (i in 0...32) {
			for (j in 0...8) {
				var c:Int = (v < 10) ? (v + 48) : (v - 10 + 65);
				v = (v + 1) & 0x0f;
				sb.addChar(c);
			}
			checkHexString(sb.toString());
		}

		eq("2147483647", BigInt.fromHex("07fffffff").toString());
		eq("-2147483648", BigInt.fromHex("f80000000").toString());

		eq("-2147483648", BigInt.fromHex("8000 0000").toString());

		eq(BigInt.fromHexSigned("080000000").toHex(), BigInt.fromHexUnsigned("80000000").toHex());
		eq(BigInt.fromHexSigned("0ffffffff").toHex(), BigInt.fromHexUnsigned("ffffffff").toHex());
		eq(BigInt.fromHexSigned("0f80000000").toHex(), BigInt.fromHexUnsigned("f80000000").toHex());
	}

	private function checkHexString(value:String):Void {
		var bi = BigInt.fromHex(value);
		eq(value.toLowerCase(), bi.toHex().toLowerCase());
		var by = bi.toBytes();
		eq(by.toHex().toLowerCase(), bi.toHex().toLowerCase());
	}

	public function bigIntDecimalStrings():Void {
		eq("0", BigInt.ZERO.toString());
		checkDecString("1");
		checkDecString("99");
		checkDecString("2147483647");
		checkDecString("2147483648");
		checkDecString("2147483649");
		checkDecString("4294967295");
		checkDecString("4294967296");
		checkDecString("4294967297");
		for (i in 0...s_powersOfTwo.length) {
			var s = s_powersOfTwo[i];
			checkDecString(s);
			checkDecString("9" + s);
			checkDecString(s + "9");
			checkDecString("9" + s + "9");
		}
		var s = "1";
		for (i in 0...100) {
			s = s + "0";
			checkDecString(s);
		}
		eq("1512366075204170929049582354406559215", BigInt.fromHex("01234567 89abcdef 01234567 89abcdef").toString());
	}

	private function checkDecString(value:String):Void {
		var biPos = BigInt.fromString(value);
		eq(value, biPos.toString());
		value = "-" + value;
		var biNeg = BigInt.fromString(value);
		eq(value, biNeg.toString());
	}

	public function testBigIntTools() {
		// Test makes copy
		var a:MutableBigInt = BigInt.fromHex("123456789abcdef0");
		var b = BigIntTools.parseValueUnsigned(a);
		eq("123456789abcdef0", b.toHex());
		a.clear();
		eq("123456789abcdef0", b.toHex());

		// Test is null
		var m_a:MutableBigInt = null;
		t(BigIntTools.isNull(m_a));
		m_a = 0;
		f(BigIntTools.isNull(m_a));

		// Cast from dynamic
		eq(null, BigIntTools.castFrom(null));
		eq(null, BigIntTools.castFrom("whee"));
		eq(null, BigIntTools.castFrom(5));
		var a1:BigInt = 0;
		eq(a1.toHex(), BigIntTools.castFrom(a1).toHex());
		var b1:MutableBigInt = 0;
		eq(b1.toHex(), BigIntTools.castFrom(b1).toHex());
		var c:BigInt_ = new BigInt_();
		eq(c, BigIntTools.castFrom(c));
		var d:MutableBigInt_ = new MutableBigInt_();
		eq(cast d, BigIntTools.castFrom(d));

		// Test is bigint
		f(BigIntTools.isBigInt("whee"));
		f(BigIntTools.isBigInt(5));
		t(BigIntTools.isBigInt(a1));
		t(BigIntTools.isBigInt(b1));

		// Test parse
		eq(BigInt.fromHex("123456789abcdef0").toHex(), BigIntTools.parseValueUnsigned("0x123456789abcdef0").toHex());
		eq(BigInt.fromString("12345678901234567890").toHex(), BigIntTools.parseValueUnsigned("12345678901234567890").toHex());
		eq(BigInt.fromHex("080000000").toHex(), BigIntTools.parseValueUnsigned("0x80000000").toHex());
	}

	public function testMultiwordArithmeticCompare():Void {
		checkCompareMultiwordArithmetic(0, fromInt(0), fromInt(0));
		checkCompareMultiwordArithmetic(-1, fromInt(0), fromInt(1));
		checkCompareMultiwordArithmetic(-1, fromInt(1), fromInt(2));
		checkCompareMultiwordArithmetic(-1, fromInt(-1), fromInt(0));
		checkCompareMultiwordArithmetic(-1, fromInt(-2), fromInt(-1));
		checkCompareMultiwordArithmetic(-1, fromInt(2147483646), fromInt(2147483647));
		checkCompareMultiwordArithmetic(-1, fromInt(-2147483648), fromInt(2147483647));
		checkCompareMultiwordArithmetic(-1, fromInt(-2147483648), fromInt(-2147483647));
		checkCompareMultiwordArithmetic(-1, fromInt(-2147483648), fromInt(-1));

		checkCompareMultiwordArithmetic(1, fromHex("00000001 80000000", 2), fromHex("00000001 7fffffff", 2));
		checkCompareMultiwordArithmetic(1, fromHex("00000001 ffffffff", 2), fromHex("00000001 00000000", 2));
	}

	private function checkCompareMultiwordArithmetic(expected:Int, a:Vector<Int>, b:Vector<Int>):Void {
		eq(expected, MultiwordArithmetic.compareSigned(a, b, a.length));
		eq(-expected, MultiwordArithmetic.compareSigned(b, a, a.length));
		var as:Int = (a[a.length - 1]) < 0 ? -1 : 1;
		var bs:Int = (b[b.length - 1]) < 0 ? -1 : 1;
		var s:Int = as * bs;
		eq(expected * s, MultiwordArithmetic.compareUnsigned(a, b, a.length));
		eq(-expected * s, MultiwordArithmetic.compareUnsigned(b, a, a.length));
	}

	public function testMultiwordArithmeticArithmeticShiftRight():Void {
		MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 2), fromInt(1, 2), 2, 1);
		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 2), fromInt(1, 2), 2, -1); // shift negative
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 2), fromInt(1, 2), 2, 32); // shift 32
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 1), fromInt(1, 2), 2, 1); // result too short
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 2), fromInt(1, 1), 2, 1); // input too short
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 2), fromInt(1, 2), 0, 1); // length too short
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// result and input overlap
		var result = fromHex("12345678 9abcdef0");
		MultiwordArithmetic.arithmeticShiftRight(result, result, 2, 4);
		assertEqualsBI(fromHex("01234567 89abcdef"), result);

		// result and input overlap and shift is 0
		var result = fromHex("12345678 9abcdef0");
		MultiwordArithmetic.arithmeticShiftRight(result, result, 2, 0);
		assertEqualsBI(fromHex("12345678 9abcdef0"), result);

		checkASR(fromHex("00000000 00000000", 2), fromHex("00000000 00000000", 2), 1);
		checkASR(fromHex("00000000 00000000", 2), fromHex("00000000 00000001", 2), 1);
		checkASR(fromHex("00000000 00000001", 2), fromHex("00000000 00000002", 2), 1);
		checkASR(fromHex("00000000 80000000", 2), fromHex("00000001 00000001", 2), 1);
		checkASR(fromHex("ffffffff ffffffff", 2), fromHex("ffffffff ffffffff", 2), 1);
		checkASR(fromHex("ffffffff ffffffff", 2), fromHex("ffffffff ffffffff", 2), 31);
		checkASR(fromHex("12345678 9abcdef0", 2), fromHex("12345678 9abcdef0", 2), 0);
		checkASR(fromHex("01234567 89abcdef", 2), fromHex("12345678 9abcdef0", 2), 4);
		checkASR(fromHex("00000001 23456789", 2), fromHex("12345678 9abcdef0", 2), 28);
		checkASR(fromHex("00000000 40000000", 2), fromHex("00000000 80000000", 2), 1);
		checkASR(fromHex("c0000000 00000000", 2), fromHex("80000000 00000000", 2), 1);
	}

	public function checkASR(expected:Vector<Int>, input:Vector<Int>, shift:Int):Void {
		var result = new Vector<Int>(input.length);
		MultiwordArithmetic.arithmeticShiftRight(result, input, input.length, shift);
		assertEqualsBI(expected, result);
	}

	public function testMultiwordArithmeticLogicalShiftRight():Void {
		MultiwordArithmetic.logicalShiftRight(fromInt(0, 2), fromInt(1, 2), 2, 1);
		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 2), fromInt(1, 2), 2, -1); // shift negative
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 2), fromInt(1, 2), 2, 32); // shift 32
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 1), fromInt(1, 2), 2, 1); // result too short
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 2), fromInt(1, 1), 2, 1); // input too short
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 2), fromInt(1, 2), 0, 1); // length too short
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// result and input overlap
		var result = fromHex("12345678 9abcdef0");
		MultiwordArithmetic.logicalShiftRight(result, result, 2, 4);
		assertEqualsBI(fromHex("01234567 89abcdef"), result);

		// result and input overlap and shift is 0
		var result = fromHex("12345678 9abcdef0");
		MultiwordArithmetic.logicalShiftRight(result, result, 2, 0);
		assertEqualsBI(fromHex("12345678 9abcdef0"), result);

		checkShiftRight(fromHex("00000000 00000000", 2), fromHex("00000000 00000000", 2), 1);
		checkShiftRight(fromHex("00000000 00000000", 2), fromHex("00000000 00000001", 2), 1);
		checkShiftRight(fromHex("00000000 00000001", 2), fromHex("00000000 00000002", 2), 1);
		checkShiftRight(fromHex("00000000 80000000", 2), fromHex("00000001 00000001", 2), 1);
		checkShiftRight(fromHex("7fffffff ffffffff", 2), fromHex("ffffffff ffffffff", 2), 1);
		checkShiftRight(fromHex("00000001 ffffffff", 2), fromHex("ffffffff ffffffff", 2), 31);
		checkShiftRight(fromHex("12345678 9abcdef0", 2), fromHex("12345678 9abcdef0", 2), 0);
		checkShiftRight(fromHex("01234567 89abcdef", 2), fromHex("12345678 9abcdef0", 2), 4);
		checkShiftRight(fromHex("00000001 23456789", 2), fromHex("12345678 9abcdef0", 2), 28);
	}

	public function checkShiftRight(expected:Vector<Int>, input:Vector<Int>, shift:Int):Void {
		var result = new Vector<Int>(input.length);
		MultiwordArithmetic.logicalShiftRight(result, input, input.length, shift);
		assertEqualsBI(expected, result);
	}

	public function testMultiwordArithmeticExtendUnsigned():Void {
		var result = new Vector<Int>(4);

		fill(result);
		MultiwordArithmetic.extendUnsigned(result, 2, fromInt(0), 1);
		assertEqualsBI(0, result, 2);
		eq(0xdeadbeef, result[2]);

		fill(result);
		MultiwordArithmetic.extendUnsigned(result, 2, fromInt(-2147483648), 1);
		assertEqualsBI(-2147483648, result, 2);
		eq(0xdeadbeef, result[2]);

		fill(result);
		MultiwordArithmetic.extendUnsigned(result, 2, fromInt(-1), 1);
		assertEqualsBI(-1, result, 2);
		eq(0xdeadbeef, result[2]);

		fill(result);
		MultiwordArithmetic.extendUnsigned(result, 4, fromHex("1 00000000"), 2);
		assertEqualsBI(fromHex("1 00000000", 4), result, 4);

		fill(result);
		MultiwordArithmetic.setFromIntUnsigned(result, 1, 123);
		MultiwordArithmetic.extendUnsigned(result, 4, result, 1);
		assertEqualsBI(123, result, 4);
	}

	public function testMultiwordArithmeticSubtractSemantics():Void {
		// All inputs can be the same
		var result = fromInt(11);
		MultiwordArithmetic.subtract(result, result, result, 1);
		assertEqualsBI(0, result);

		// Bounds
		MultiwordArithmetic.subtract(fromInt(0, 2), fromInt(0, 2), fromInt(0, 2), 2);
		try {
			MultiwordArithmetic.subtract(fromInt(0, 1), fromInt(0, 2), fromInt(0, 2), 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.subtract(fromInt(0, 2), fromInt(0, 1), fromInt(0, 2), 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.subtract(fromInt(0, 2), fromInt(0, 2), fromInt(0, 1), 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.subtract(fromInt(0, 2), fromInt(0, 2), fromInt(0, 2), 0);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
	}

	public function testMultiwordArithmeticSubtractCarry():Void {
		var result = new Vector<Int>(2);
		var c:Int;

		eq(0, MultiwordArithmetic.subtract(result, fromInt(0), fromInt(0), 1));
		eq(1, MultiwordArithmetic.subtract(result, fromInt(0), fromInt(1), 1));
		eq(0, MultiwordArithmetic.subtract(result, fromInt(-1), fromInt(-1), 1));
		eq(1, MultiwordArithmetic.subtract(result, fromInt(-2), fromInt(-1), 1));
		eq(0, MultiwordArithmetic.subtract(result, fromInt(-2147483648), fromInt(-2147483648), 1));
		eq(1, MultiwordArithmetic.subtract(result, fromInt(2147483647), fromInt(-2147483648), 1));

		eq(0, MultiwordArithmetic.subtract(result, fromHex("00000000 00000001", 2), fromInt(1, 2), 2));
		eq(0, MultiwordArithmetic.subtract(result, fromHex("00000001 00000000", 2), fromInt(1, 2), 2));
		eq(1, MultiwordArithmetic.subtract(result, fromHex("00000000 00000000", 2), fromInt(1, 2), 2));
	}

	public function testMultiwordArithmeticAddSemantics():Void {
		// All inputs can be the same
		var result = fromInt(11);
		MultiwordArithmetic.add(result, result, result, 1);
		assertEqualsBI(22, result);

		// Bounds
		MultiwordArithmetic.add(fromInt(0, 2), fromInt(0, 2), fromInt(0, 2), 2);
		try {
			MultiwordArithmetic.add(fromInt(0, 1), fromInt(0, 2), fromInt(0, 2), 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.add(fromInt(0, 2), fromInt(0, 1), fromInt(0, 2), 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.add(fromInt(0, 2), fromInt(0, 2), fromInt(0, 1), 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.add(fromInt(0, 2), fromInt(0, 2), fromInt(0, 2), 0);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
	}

	public function testMultiwordArithmeticAddCarry():Void {
		var result = new Vector<Int>(2);
		var c:Int;

		eq(0, MultiwordArithmetic.add(result, fromInt(0), fromInt(0), 1));
		eq(0, MultiwordArithmetic.add(result, fromInt(0), fromInt(1), 1));
		eq(0, MultiwordArithmetic.add(result, fromInt(1), fromInt(2147483647), 1));
		eq(0, MultiwordArithmetic.add(result, fromInt(1), fromInt(-2), 1));
		eq(1, MultiwordArithmetic.add(result, fromInt(1), fromInt(-1), 1));
		eq(1, MultiwordArithmetic.add(result, fromInt(-2147483648), fromInt(-2147483648), 1));

		eq(0, MultiwordArithmetic.add(result, fromHex("ffffffff fffffffe"), fromInt(1, 2), 2));
		eq(0, MultiwordArithmetic.add(result, fromHex("fffffffe ffffffff"), fromInt(1, 2), 2));
		eq(1, MultiwordArithmetic.add(result, fromHex("ffffffff ffffffff"), fromInt(1, 2), 2));
	}

	public function testMultiwordArithmeticAddAndSubtract():Void {
		checkAddMultiwordArithmetic(fromInt(0), fromInt(0), fromInt(0));
		checkAddMultiwordArithmetic(fromInt(1), fromInt(0), fromInt(1));
		checkAddMultiwordArithmetic(fromInt(2), fromInt(1), fromInt(1));
		checkAddMultiwordArithmetic(fromInt(-1), fromInt(-1), fromInt(0));
		checkAddMultiwordArithmetic(fromInt(0), fromInt(-1), fromInt(1));
		checkAddMultiwordArithmetic(fromInt(1), fromInt(-1), fromInt(2));
		checkAddMultiwordArithmetic(fromInt(-2), fromInt(-1), fromInt(-1));

		checkAddMultiwordArithmetic(fromHex("00000000 00000000 80000000", 3), fromHex("00000000 00000000 7fffffff", 3), fromInt(1, 3));
		checkAddMultiwordArithmetic(fromHex("00000000 80000000 00000000", 3), fromHex("00000000 7fffffff ffffffff", 3), fromInt(1, 3));
		checkAddMultiwordArithmetic(fromHex("80000000 00000000 00000000", 3), fromHex("7fffffff ffffffff ffffffff", 3), fromInt(1, 3));
		checkAddMultiwordArithmetic(fromHex("ffffffff ffffffff 00000000", 3), fromHex("ffffffff fffffffe ffffffff", 3), fromInt(1, 3));
		checkAddMultiwordArithmetic(fromHex("00000001 00000000 00000000 00000000", 4), fromHex("00000000 ffffffff ffffffff ffffffff", 4),
			fromHex("00000000 00000000 00000001", 4));
		checkAddMultiwordArithmetic(fromHex("00000001 00000000 00000000 00000000", 4), fromHex("00000000 ffffffff ffffffff 00000000", 4),
			fromHex("00000000 00000001 00000000", 4));
		checkAddMultiwordArithmetic(fromHex("00000001 00000000 00000000 00000000", 4), fromHex("00000000 ffffffff 00000000 00000000", 4),
			fromHex("00000001 00000000 00000000", 4));

		checkAddMultiwordArithmetic(fromHex("23456789"), fromHex("12345678"), fromHex("11111111"));
		checkAddMultiwordArithmetic(fromHex("2345678923456789"), fromHex("1234567812345678"), fromHex("1111111111111111"));
		checkAddMultiwordArithmetic(fromHex("234567892345678923456789"), fromHex("123456781234567812345678"), fromHex("111111111111111111111111"));

		checkAddMultiwordArithmetic(fromHex("1234567823456789"), fromHex("1234567812345678"), fromHex("11111111", 2));
		checkAddMultiwordArithmetic(fromHex("123456781234567823456789"), fromHex("123456781234567812345678"), fromHex("11111111", 3));
		checkAddMultiwordArithmetic(fromHex("2345678912345678"), fromHex("1234567812345678"), fromHex("1111111100000000"));
		checkAddMultiwordArithmetic(fromHex("234567891234567812345678"), fromHex("123456781234567812345678"), fromHex("111111110000000000000000"));
		checkAddMultiwordArithmetic(fromHex("234567891234567823456789"), fromHex("123456781234567812345678"), fromHex("111111110000000011111111"));
	}

	private function checkAddMultiwordArithmetic(expected:Vector<Int>, a:Vector<Int>, b:Vector<Int>):Void {
		var negExpected = new Vector<Int>(expected.length);
		var negA = new Vector<Int>(a.length);
		var negB = new Vector<Int>(b.length);

		MultiwordArithmetic.negate(negExpected, expected, expected.length);
		MultiwordArithmetic.negate(negA, a, a.length);
		MultiwordArithmetic.negate(negB, b, b.length);

		checkAddCommuteMultiwordArithmetic(expected, a, b);
		checkAddCommuteMultiwordArithmetic(negExpected, negA, negB);
		checkAddCommuteMultiwordArithmetic(b, expected, negA);
		checkAddCommuteMultiwordArithmetic(a, expected, negB);
	}

	private function checkAddCommuteMultiwordArithmetic(expected:Vector<Int>, a:Vector<Int>, b:Vector<Int>):Void {
		checkAddSingleMultiwordArithmetic(expected, a, b);
		checkAddSingleMultiwordArithmetic(expected, b, a);
	}

	private function checkAddSingleMultiwordArithmetic(expected:Vector<Int>, a:Vector<Int>, b:Vector<Int>):Void {
		var result = new Vector<Int>(expected.length);
		var c = MultiwordArithmetic.add(result, a, b, expected.length);
		assertEqualsBI(expected, result);
		c = MultiwordArithmetic.subtract(result, expected, b, expected.length);
		assertEqualsBI(a, result);
	}

	public function testMultiwordArithmeticNegate():Void {
		checkNegate(fromInt(0), fromInt(0));
		checkNegate(fromInt(1), fromInt(-1));
		checkNegate(fromInt(100), fromInt(-100));
		checkNegate(fromHex("80000000"), fromInt(-2147483648));
		checkNegate(fromHex("8000000000000000"), fromHex("8000000000000000"));
		checkNegate(fromHex("edcba98800000000"), fromHex("1234567800000000"));
	}

	private function checkNegate(expected:Vector<Int>, value:Vector<Int>):Void {
		var temp = new Vector<Int>(value.length);
		MultiwordArithmetic.negate(temp, value, value.length);
		assertEqualsBI(expected, temp);
		MultiwordArithmetic.negate(temp, temp, temp.length);
		assertEqualsBI(value, temp);
	}

	public function testMultiwordArithmeticToDecimal():Void {
		var value = new Vector<Int>(1);
		value[0] = 0;
		eq("0", MultiwordArithmetic.toDecimalUnsigned(value, value.length));
		eq("0", MultiwordArithmetic.toDecimalSigned(value, value.length));

		checkDecStringMultiwordArithmetic("1");
		checkDecStringMultiwordArithmetic("99");
		checkDecStringMultiwordArithmetic("2147483647");
		checkDecStringMultiwordArithmetic("2147483648");
		checkDecStringMultiwordArithmetic("2147483649");
		checkDecStringMultiwordArithmetic("4294967295");
		checkDecStringMultiwordArithmetic("4294967296");
		checkDecStringMultiwordArithmetic("4294967297");
		// TODO
		/*for (i in 0 ... s_powersOfTwo.length)
			{
				var s = s_powersOfTwo[i];
				checkDecStringMultiwordArithmetic(s);
				checkDecStringMultiwordArithmetic("9" + s);
				checkDecStringMultiwordArithmetic(s + "9");
				checkDecStringMultiwordArithmetic("9" + s + "9");
		}*/
		var s = "1";
		for (i in 0...100) {
			s = s + "0";
			checkDecStringMultiwordArithmetic(s);
		}
		eq("1512366075204170929049582354406559215", MultiwordArithmetic.toDecimalUnsigned(fromHex("01234567 89abcdef 01234567 89abcdef"), 4));
	}

	private function checkDecStringMultiwordArithmetic(value:String):Void {
		// TODO
		/*var biPos = fromString(value);
			assertEquals(value, biPos.toString());
			value = "-" + value;
			var biNeg = fromString(value);
			assertEquals(value, biNeg.toString()); */
	}

	public function testMultiwordArithmeticIsZero():Void {
		eq(true, MultiwordArithmetic.isZero(fromInt(0), 1));
		eq(false, MultiwordArithmetic.isZero(fromInt(1), 1));
		eq(false, MultiwordArithmetic.isZero(fromInt(-1), 1));
	}

	public function testMultiwordArithmeticSetFromHexUnsignedInitializesWordsWithoutHexDigits():Void {
		var result = new Vector<Int>(4);
		MultiwordArithmetic.setFromHexUnsigned(result, 4, "abc");
		eq(2748, result[0]);
		eq(0, result[1]);
		eq(0, result[2]);
		eq(0, result[3]);
	}

	public function testMultiwordArithmeticSetFromHexUnsigned():Void {
		var value = fromInt(0);

		try {
			MultiwordArithmetic.setFromHexUnsigned(value, 1, null);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.setFromHexUnsigned(value, 1, "");
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.setFromHexUnsigned(null, 1, "0");
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.setFromHexUnsigned(value, 2, "0"); // buffer too small
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.setFromHexUnsigned(value, 1, "0g0"); // invalid char
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		value = new Vector<Int>(3);
		f(MultiwordArithmetic.setFromHexUnsigned(value, 0, "0"));
		f(MultiwordArithmetic.setFromHexUnsigned(value, 1, "1 00000000"));
		f(MultiwordArithmetic.setFromHexUnsigned(value, 2, "1 00000000 00000000"));
		t(MultiwordArithmetic.setFromHexUnsigned(value, 1, "00000000 00000000 000000cb"));
		eq(203, value[0]);

		checkSetFromHexUnsigned("00000000", "0");
		checkSetFromHexUnsigned("0000000000000000", "0");
		checkSetFromHexUnsigned("000000000000000000000000", "0");

		checkSetFromHexUnsigned("00000001", "1");
		checkSetFromHexUnsigned("0000000100000000", "1 00000000");
		checkSetFromHexUnsigned("000000000000000100000000", "1 00000000");

		checkSetFromHexUnsigned("7fffffff", "7fffffff");
		checkSetFromHexUnsigned("7fffffffffffffff", "7fffffff ffffffff");
		checkSetFromHexUnsigned("7fffffffffffffffffffffff", "7fffffff ffffffff ffffffff");

		checkSetFromHexUnsigned("80000000", "80000000");
		checkSetFromHexUnsigned("8000000000000000", "80000000 00000000");
		checkSetFromHexUnsigned("800000000000000000000000", "80000000 00000000 00000000");

		checkSetFromHexUnsigned("ffffffff", "ffffffff");
		checkSetFromHexUnsigned("ffffffffffffffff", "ffffffff ffffffff");
		checkSetFromHexUnsigned("ffffffffffffffffffffffff", "ffffffff ffffffff ffffffff");
		checkSetFromHexUnsigned("0000000000000000ffffffff", "ffffffff");

		checkSetFromHexUnsigned("00abcdef", "ABCDEF");
		checkSetFromHexUnsigned("00abcdef", "abcdef");
	}

	private function checkSetFromHexUnsigned(expected:String, input:String):Void {
		var len:Int = (expected.length + 7) >> 3;
		var result = new Vector<Int>(len);
		t(MultiwordArithmetic.setFromHexUnsigned(result, len, input));
		var hex = MultiwordArithmetic.toHex(result, len);
		eq(expected, hex);
	}

	public function testMultiwordArithmeticMultiplySemantics():Void {
		var result:Vector<Int>;

		// if result of multiplication is an input, should throw an exception
		result = fromInt(2, 4);
		try {
			MultiwordArithmetic.multiplyUnsigned(result, result, 1, fromInt(2), 1);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.multiplyUnsigned(result, fromInt(2), 1, result, 1);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.multiplyIntUnsigned(result, result, 1, 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// Multiplication of same inputs is ok
		var a = fromInt(2);
		result = fromInt(0, 2);
		MultiwordArithmetic.multiplyUnsigned(result, a, 1, a, 1);
		assertEqualsBI(4, result);

		// check for result length too short
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0), fromInt(2), 1, fromInt(2), 1);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.multiplyIntUnsigned(fromInt(0), fromInt(2), 1, 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// check input lengths
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0, 2), fromInt(2), 0, fromInt(2), 1);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0, 2), fromInt(2), 1, fromInt(2), 0);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.multiplyIntUnsigned(fromInt(0, 2), fromInt(2), 0, 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// check input bounds
		MultiwordArithmetic.multiplyUnsigned(fromInt(0, 4), fromInt(2, 2), 2, fromInt(2, 2), 2);
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0, 4), fromInt(2, 1), 2, fromInt(2, 2), 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0, 4), fromInt(2, 2), 2, fromInt(2, 1), 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.multiplyIntUnsigned(fromInt(0, 4), fromInt(2, 1), 2, 2);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
	}

	public function testMultiwordArithmeticDivideSemantics():Void {
		var dividend = fromInt(0);
		var divisor:Vector<Int>;
		var quotient:Vector<Int>;
		var remainder = fromInt(0);
		var remainderInt:Int;
		var work = new Vector<Int>(10);

		// check quotient overlaps with dividend, divisor < 65536 case
		quotient = fromInt(12345);
		remainderInt = MultiwordArithmetic.divideIntUnsigned(quotient, quotient.length, 11, quotient, work);
		assertEqualsBI(1122, quotient);
		eq(3, remainderInt);
		quotient = fromInt(12345);
		MultiwordArithmetic.divideUnsigned(quotient, quotient.length, fromInt(11), 1, quotient, remainder, work);
		assertEqualsBI(1122, quotient);
		assertEqualsBI(3, remainder);

		// check quotient overlaps with dividend, divisor >= 65536 case
		quotient = fromInt(721018);
		divisor = fromInt(65537);
		MultiwordArithmetic.divideUnsigned(quotient, quotient.length, divisor, divisor.length, quotient, remainder, work);
		assertEqualsBI(11, quotient);
		assertEqualsBI(111, remainder);

		// check quotient overlaps with dividend, special case 1
		quotient = fromInt(0);
		divisor = fromInt(11);
		MultiwordArithmetic.divideUnsigned(quotient, quotient.length, divisor, divisor.length, quotient, remainder, work);
		assertEqualsBI(0, quotient);
		assertEqualsBI(0, remainder);

		// check quotient overlaps with dividend, special case 2
		quotient = fromInt(7);
		MultiwordArithmetic.divideUnsigned(quotient, quotient.length, fromInt(1), 1, quotient, remainder, work);
		assertEqualsBI(7, quotient);
		assertEqualsBI(0, remainder);

		// check quotient overlaps with dividend, special case 3
		quotient = fromInt(11);
		remainder = fromInt(0, 2);
		MultiwordArithmetic.divideUnsigned(quotient, quotient.length, fromHex("1 00000000"), 2, quotient, remainder, work);
		assertEqualsBI(0, quotient);
		assertEqualsBI(11, remainder);

		// check quotient overlaps with divisor, divisor < 65536 case
		quotient = fromInt(11);
		MultiwordArithmetic.divideUnsigned(fromInt(12345), 1, quotient, quotient.length, quotient, remainder, work);
		assertEqualsBI(1122, quotient);
		assertEqualsBI(3, remainder);

		// check quotient overlaps with divisor, divisor >= 65536 case
		quotient = fromInt(65537);
		MultiwordArithmetic.divideUnsigned(fromInt(721018), 1, quotient, quotient.length, quotient, remainder, work);
		assertEqualsBI(11, quotient);
		assertEqualsBI(111, remainder);

		// check quotient overlaps with divisor, special case 1
		quotient = fromInt(10);
		MultiwordArithmetic.divideUnsigned(fromInt(0), 1, quotient, quotient.length, quotient, remainder, work);
		assertEqualsBI(0, quotient);
		assertEqualsBI(0, remainder);

		// check quotient overlaps with divisor, special case 2
		quotient = fromInt(1);
		MultiwordArithmetic.divideUnsigned(fromInt(7), 1, quotient, quotient.length, quotient, remainder, work);
		assertEqualsBI(7, quotient);
		assertEqualsBI(0, remainder);

		// check quotient overlaps with divisor, special case 3
		quotient = fromHex("1 00000000");
		MultiwordArithmetic.divideUnsigned(fromInt(11), 1, quotient, quotient.length, quotient, remainder, work);
		assertEqualsBI(0, quotient, 1);
		assertEqualsBI(11, remainder);

		// check remainder overlaps with dividend, divisor < 65536 case
		quotient = fromInt(0);
		remainder = fromInt(12345);
		MultiwordArithmetic.divideUnsigned(remainder, remainder.length, fromInt(11), 1, quotient, remainder, work);
		assertEqualsBI(1122, quotient);
		assertEqualsBI(3, remainder);

		// check remainder overlaps with dividend, divisor >= 65536 case
		remainder = fromInt(721018);
		MultiwordArithmetic.divideUnsigned(remainder, remainder.length, fromInt(65537), 1, quotient, remainder, work);
		assertEqualsBI(11, quotient);
		assertEqualsBI(111, remainder);

		// check remainder overlaps with dividend, special case 1
		remainder = fromInt(0);
		MultiwordArithmetic.divideUnsigned(remainder, remainder.length, fromInt(10), 1, quotient, remainder, work);
		assertEqualsBI(0, quotient);
		assertEqualsBI(0, remainder);

		// check remainder overlaps with dividend, special case 2
		remainder = fromInt(7);
		MultiwordArithmetic.divideUnsigned(remainder, remainder.length, fromInt(1), 1, quotient, remainder, work);
		assertEqualsBI(7, quotient);
		assertEqualsBI(0, remainder);

		// check remainder overlaps with dividend, special case 3
		remainder = fromInt(11, 2);
		MultiwordArithmetic.divideUnsigned(remainder, 1, fromHex("1 00000000"), 2, quotient, remainder, work);
		assertEqualsBI(0, quotient);
		assertEqualsBI(11, remainder);

		// check remainder overlaps with divisor, divisor < 65536 case
		remainder = fromInt(11);
		MultiwordArithmetic.divideUnsigned(fromInt(12345), 1, remainder, remainder.length, quotient, remainder, work);
		assertEqualsBI(1122, quotient);
		assertEqualsBI(3, remainder);

		// check remainder overlaps with divisor, divisor >= 65536 case
		remainder = fromInt(65537);
		MultiwordArithmetic.divideUnsigned(fromInt(721018), 1, remainder, remainder.length, quotient, remainder, work);
		assertEqualsBI(11, quotient);
		assertEqualsBI(111, remainder);

		// check remainder overlaps with divisor, special case 1
		remainder = fromInt(10);
		MultiwordArithmetic.divideUnsigned(fromInt(0), 1, remainder, remainder.length, quotient, remainder, work);
		assertEqualsBI(0, quotient);
		assertEqualsBI(0, remainder);

		// check remainder overlaps with divisor, special case 2
		remainder = fromInt(1);
		MultiwordArithmetic.divideUnsigned(fromInt(7), 1, remainder, remainder.length, quotient, remainder, work);
		assertEqualsBI(7, quotient);
		assertEqualsBI(0, remainder);

		// check remainder overlaps with divisor, special case 3
		remainder = fromHex("1 00000000");
		MultiwordArithmetic.divideUnsigned(fromInt(11), 1, remainder, remainder.length, quotient, remainder, work);
		assertEqualsBI(0, quotient);
		assertEqualsBI(11, remainder);

		// quotient and remainder cannot be the same object
		try {
			MultiwordArithmetic.divideUnsigned(fromInt(1), 1, fromInt(10), 1, quotient, quotient, work);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// quotient cannot be null
		try {
			MultiwordArithmetic.divideIntUnsigned(fromInt(1), 1, 10, null, work);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.divideUnsigned(fromInt(1), 1, fromInt(10), 1, null, remainder, work);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// work cannot be null
		try {
			MultiwordArithmetic.divideIntUnsigned(fromInt(1), 1, 10, quotient, null);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.divideUnsigned(fromInt(1), 1, fromInt(10), 1, quotient, remainder, null);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// remainder can be null
		MultiwordArithmetic.divideUnsigned(fromInt(1), 1, fromInt(10), 1, quotient, null, work);
		assertEqualsBI(0, quotient);

		// dividend and divisor can be the same object
		divisor = fromInt(10);
		MultiwordArithmetic.divideUnsigned(divisor, divisor.length, divisor, divisor.length, quotient, remainder, work);
		assertEqualsBI(1, quotient);
		assertEqualsBI(0, remainder);

		// work may not overlap any input
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, dividend);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, divisor);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, quotient);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, remainder);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// bounds
		MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, work);
		try {
			MultiwordArithmetic.divideUnsigned(dividend, 0, divisor, divisor.length, quotient, remainder, work);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, 0, quotient, remainder, work);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, new Vector<Int>(0), remainder, work);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}

		// division by zero
		try {
			MultiwordArithmetic.divideIntUnsigned(dividend, dividend.length, 0, quotient, work);
		} catch (e) {
			eq(cast BigIntExceptions.DIVISION_BY_ZERO, e.message);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, fromInt(0), 1, quotient, remainder, work);
		} catch (e) {
			eq(cast BigIntExceptions.DIVISION_BY_ZERO, e.message);
		}

		// divisor with leading 0
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, fromHex("1", 2), 2, quotient, remainder, work);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
	}

	public function testMultiwordArithmeticMultiplicationAndDivision():Void {
		checkLinearEqIntMultiwordArithmetic(fromInt(0), 0, fromInt(0), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(0), 1, fromInt(0), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(0), 0, fromInt(1), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(0), 100, fromInt(0), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(1), 1, fromInt(1), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(1), 2, fromInt(0), 1);
		checkLinearEqIntMultiwordArithmetic(fromInt(2), 1, fromInt(2), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(2), 3, fromInt(0), 2);
		checkLinearEqIntMultiwordArithmetic(fromInt(3), 3, fromInt(1), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(4), 2, fromInt(2), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(4), 3, fromInt(1), 1);
		checkLinearEqIntMultiwordArithmetic(fromInt(5), 3, fromInt(1), 2);
		checkLinearEqIntMultiwordArithmetic(fromInt(6), 3, fromInt(2), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(6), 2, fromInt(3), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("12A05F2001"), 81, fromInt(987654321), 0);

		checkLinearEqMultiwordArithmetic(fromHex("0 fffffffe 00000001"), fromHex("0 ffffffff"), fromHex("0 ffffffff"), fromInt(0)); // exercises qhat = 65536
		checkLinearEqMultiwordArithmetic(fromHex("00003fff c0000000 7fff8000 00000000"), fromHex("7fff8000 00000000"), fromHex("00008000 00000001"),
			fromInt(0));

		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 1, fromInt(2147483647), 0);
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 10, fromInt(214748364), 7);
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 100, fromInt(21474836), 47);
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 1000, fromInt(2147483), 647);
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 10000, fromInt(214748), 3647);
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 100000, fromInt(21474), 83647); // exercises rhat >= 65536
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 1000000, fromInt(2147), 483647);
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 10000000, fromInt(214), 7483647);
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 100000000, fromInt(21), 47483647);
		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 1000000000, fromInt(2), 147483647);

		checkLinearEqIntMultiwordArithmetic(fromInt(2147483647), 2147483647, fromInt(1), 0); // exercises use of uninitialized quotient data

		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 1, fromHex("100000000"), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 10, fromInt(429496729), 6);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 100, fromInt(42949672), 96);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 1000, fromInt(4294967), 296);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 10000, fromInt(429496), 7296);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 100000, fromInt(42949), 67296); // exercises rhat >= 65536
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 1000000, fromInt(4294), 967296);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 10000000, fromInt(429), 4967296);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 100000000, fromInt(42), 94967296);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000000"), 1000000000, fromInt(4), 294967296);
		checkLinearEqMultiwordArithmetic(fromHex("100000000"), fromHex("2540BE400"), fromInt(0), fromHex("100000000"));

		checkLinearEqIntMultiwordArithmetic(fromHex("08000"), 1, fromHex("08000"), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("080000000"), 1, fromHex("080000000"), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("0800000000000"), 1, fromHex("0800000000000"), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("08000000000000000"), 1, fromHex("08000000000000000"), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("10001"), 2, fromHex("08000"), 1);
		checkLinearEqIntMultiwordArithmetic(fromHex("100000001"), 2, fromHex("080000000"), 1);
		checkLinearEqIntMultiwordArithmetic(fromHex("1000000000001"), 2, fromHex("0800000000000"), 1);
		checkLinearEqIntMultiwordArithmetic(fromHex("10000000000000001"), 2, fromHex("08000000000000000"), 1);

		checkLinearEqIntMultiwordArithmetic(fromHex("0ffffffff"), 1, fromHex("0ffffffff"), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("0ffffffffffffffff"), 1, fromHex("0ffffffffffffffff"), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("0ffffffffffffffffffffffff"), 1, fromHex("0ffffffffffffffffffffffff"), 0);
		checkLinearEqIntMultiwordArithmetic(fromHex("0ffffffff"), 2, fromHex("07fffffff"), 1);
		checkLinearEqIntMultiwordArithmetic(fromHex("0ffffffffffffffff"), 2, fromHex("07fffffffffffffff"), 1);
		checkLinearEqIntMultiwordArithmetic(fromHex("0ffffffffffffffffffffffff"), 2, fromHex("07fffffffffffffffffffffff"), 1);

		// exercise quotient with high bit set when length of divisor == length of dividend and divisor >= 65536
		checkLinearEqMultiwordArithmetic(fromHex("4000000000000000"), fromHex("080000000"), fromHex("080000000"),
			fromInt(0)); // exercises uninitialized work data
		checkLinearEqMultiwordArithmetic(fromHex("4000000080000000"), fromHex("080000001"), fromHex("080000000"), fromInt(0));
		checkLinearEqMultiwordArithmetic(fromHex("4000000100000000"), fromHex("080000001"), fromHex("080000000"), fromHex("080000000"));
		checkLinearEqMultiwordArithmetic(fromHex("40000000ffffffff"), fromHex("080000001"), fromHex("080000000"), fromHex("7fffffff"));
		checkLinearEqMultiwordArithmetic(fromHex("4000000100000001"), fromHex("080000001"), fromHex("080000001"), fromInt(0));

		checkLinearEqMultiwordArithmetic(fromHex("08000"), fromHex("0800000001"), fromHex("0"), fromHex("08000"));
		// these exercise the qhat reduction path
		checkLinearEqMultiwordArithmetic(fromHex("080000000"), fromHex("080000001"), fromHex("0"), fromHex("080000000"));
		checkLinearEqMultiwordArithmetic(fromHex("0800080010000"), fromHex("080000001"), fromHex("10000"), fromHex("080000000"));
		checkLinearEqMultiwordArithmetic(fromHex("0800100010001"), fromHex("080000001"), fromHex("10001"), fromHex("080000000"));
		checkLinearEqMultiwordArithmetic(fromHex("08000000180000000"), fromHex("080000001"), fromHex("100000000"), fromHex("080000000"));
		checkLinearEqMultiwordArithmetic(fromHex("08000000200000001"), fromHex("080000001"), fromHex("100000001"), fromHex("080000000"));

		// this exercises long division with a quotient with high bit set
		checkLinearEqMultiwordArithmetic(fromHex("08000000180000000"), fromHex("100000000"), fromHex("080000001"), fromHex("080000000"));

		// these exercise the "add back" path
		checkLinearEqMultiwordArithmetic(fromHex("7fff800000000000"), fromHex("0800000000001"), fromHex("0fffe"), fromHex("7fffffff0002"));
		checkLinearEqMultiwordArithmetic(fromHex("7fffffff800000010000000000000000"), fromHex("0800000008000000200000005"), fromHex("0fffffffd"),
			fromHex("080000000800000010000000f"));

		checkLinearEqMultiwordArithmetic(fromInt(1), fromHex("100000000"), fromInt(0), fromInt(1));
	}

	private function checkLinearEqIntMultiwordArithmetic(y:Vector<Int>, a:Int, x:Vector<Int>, b:Int):Void {
		// checks that y = ax + b
		// assertBIEqualsBI(y, a * x + b);
		var y_b = new Vector<Int>(y.length);
		MultiwordArithmetic.subtract(y_b, y, fromInt(b, y.length), y.length);
		checkMultiplyIntMultiwordArithmetic(x, a, y_b);
		if (a != 0) {
			checkDivIntMultiwordArithmetic(y, a, x, b);
		}
		checkLinearEqMultiwordArithmetic(y, fromInt(a), x, fromInt(b));
	}

	private function checkLinearEqMultiwordArithmetic(y:Vector<Int>, a:Vector<Int>, x:Vector<Int>, b:Vector<Int>):Void {
		// checks that y = ax + b
		// assertBIEqualsBI(y, a * x + b);
		var bExt = new Vector<Int>(y.length);
		MultiwordArithmetic.extendUnsigned(bExt, y.length, b, b.length);
		var y_b = new Vector<Int>(y.length);
		MultiwordArithmetic.subtract(y_b, y, bExt, y.length);
		checkMultiplyMultiwordArithmetic(a, x, y_b);
		if (!MultiwordArithmetic.isZero(a, a.length)) {
			checkDivMultiwordArithmetic(y, a, x, b);
		}
		// if we have n / d = q + r / d, then n / q = n * d / (n - r)
		if (!MultiwordArithmetic.isZero(y_b, y_b.length)) {
			var temp = new Vector<Int>(y.length + a.length);
			MultiwordArithmetic.multiplyUnsigned(temp, y, y.length, a, a.length);
			var y_bLength = MultiwordArithmetic.getLengthUnsigned(y_b, y_b.length);
			var q2 = new Vector<Int>(MultiwordArithmetic.getDivisionQuotientLengthUnsigned(temp.length, y_bLength));
			var work = new Vector<Int>(temp.length + y_bLength + 1);
			MultiwordArithmetic.divideUnsigned(temp, temp.length, y_b, y_bLength, q2, null, work);

			var r2Length = ((q2.length + x.length) > y.length) ? (q2.length + x.length) : y.length;
			temp = new Vector<Int>(r2Length);
			MultiwordArithmetic.multiplyUnsigned(temp, q2, q2.length, x, x.length);
			MultiwordArithmetic.extendUnsigned(temp, r2Length, temp, q2.length + x.length);
			var r2 = new Vector<Int>(r2Length);
			MultiwordArithmetic.extendUnsigned(r2, r2Length, y, y.length);
			MultiwordArithmetic.subtract(r2, r2, temp, r2Length);
			checkDivMultiwordArithmetic(y, x, q2, r2);
		}
	}

	private function checkMultiplyIntMultiwordArithmetic(a:Vector<Int>, b:Int, expected:Vector<Int>):Void {
		// TODO
		checkMultiplyMultiwordArithmetic(a, fromInt(b), expected);
	}

	private function checkMultiplyMultiwordArithmetic(a:Vector<Int>, b:Vector<Int>, expected:Vector<Int>):Void {
		checkMultiplyCommuteMultiwordArithmetic(a, b, expected);
		// checkMultiplyCommute(-a,  b, -expected);
		// checkMultiplyCommute( a, -b, -expected);
		// checkMultiplyCommute(-a, -b,  expected);
	}

	private function checkMultiplyCommuteMultiwordArithmetic(a:Vector<Int>, b:Vector<Int>, expected:Vector<Int>):Void {
		checkMultiplySingleMultiwordArithmetic(a, b, expected);
		checkMultiplySingleMultiwordArithmetic(b, a, expected);
	}

	private function checkMultiplySingleMultiwordArithmetic(a:Vector<Int>, b:Vector<Int>, expected:Vector<Int>):Void {
		var result = new Vector<Int>(a.length + b.length);
		MultiwordArithmetic.multiplyUnsigned(result, a, a.length, b, b.length);
		var expectedExt = new Vector<Int>(result.length);
		MultiwordArithmetic.extendUnsigned(expectedExt, result.length, expected, expected.length);
		assertEqualsBI(expectedExt, result, result.length);
	}

	private function checkDivIntMultiwordArithmetic(dividend:Vector<Int>, divisor:Int, expectedQuotient:Vector<Int>, expectedRemainder:Int):Void {
		var quotient = new Vector<Int>(MultiwordArithmetic.getDivisionQuotientLengthUnsigned(dividend.length, 1));
		var work = new Vector<Int>(dividend.length + 1 + 1);
		var remainder:Int = MultiwordArithmetic.divideIntUnsigned(dividend, dividend.length, divisor, quotient, work);
		eq(expectedRemainder, remainder);
		assertEqualsBI(expectedQuotient, quotient, MultiwordArithmetic.getLengthUnsigned(quotient, quotient.length));

		checkDivMultiwordArithmetic(dividend, fromInt(divisor), expectedQuotient, fromInt(expectedRemainder));
	}

	private function checkDivMultiwordArithmetic(dividend:Vector<Int>, divisor:Vector<Int>, expectedQuotient:Vector<Int>, expectedRemainder:Vector<Int>):Void {
		checkDivSingleMultiwordArithmetic(dividend, divisor, expectedQuotient, expectedRemainder);
		// checkDivSingle( dividend, -divisor, -expectedQuotient,  expectedRemainder);
		// checkDivSingle(-dividend,  divisor, -expectedQuotient, -expectedRemainder);
		// checkDivSingle(-dividend, -divisor,  expectedQuotient, -expectedRemainder);
	}

	private function checkDivSingleMultiwordArithmetic(dividend:Vector<Int>, divisor:Vector<Int>, expectedQuotient:Vector<Int>,
			expectedRemainder:Vector<Int>):Void {
		var quotient = new Vector<Int>(MultiwordArithmetic.getDivisionQuotientLengthUnsigned(dividend.length, divisor.length));
		var remainder = new Vector<Int>(divisor.length);
		var work = new Vector<Int>(dividend.length + divisor.length + 1);
		MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, work);
		assertEqualsBI(expectedRemainder, remainder, MultiwordArithmetic.getLengthUnsigned(remainder, remainder.length));
		assertEqualsBI(expectedQuotient, quotient, MultiwordArithmetic.getLengthUnsigned(quotient, quotient.length));
	}

	private static function fromInt(value:Int, length:Int = 1):Vector<Int> {
		var result = new Vector<Int>(length);
		MultiwordArithmetic.setFromIntUnsigned(result, length, value);
		return result;
	}

	private static function fromHex(value:String, length:Int = 1):Vector<Int> {
		while (true) {
			var result = new Vector<Int>(length);
			if (!MultiwordArithmetic.setFromHexUnsigned(result, length, value)) {
				++length;
				continue;
			}
			return result;
		}
	}

	private static function fill(value:Vector<Int>):Void {
		for (i in 0...value.length) {
			value.set(i, 0xdeadbeef);
		}
	}

	public function assertEqualsBI(expected:Dynamic, actual:Dynamic, length:Int = 0, ?c:PosInfos):Void {
		if (Std.isOfType(actual, BigInt_)) {
			var e = Std.downcast(expected, BigInt_);
			if (e == null) {
				if (Std.isOfType(expected, Int)) {
					e = BigInt.fromInt(cast(expected, Int));
				}
			}
			eq(e.toHex(), cast(actual, BigInt_).toHex(), c);
			return;
		}

		var vActual:Vector<Int> = cast actual;
		var vExpected:Vector<Int> = null;
		if (Std.isOfType(expected, Int)) {
			vExpected = new Vector<Int>(vActual.length);
			MultiwordArithmetic.setFromIntUnsigned(vExpected, vExpected.length, cast(expected, Int));
		} else {
			vExpected = cast expected;
		}
		eq(MultiwordArithmetic.toHex(vExpected, (length != 0) ? length : vExpected.length),
			MultiwordArithmetic.toHex(vActual, (length != 0) ? length : vActual.length), c);
		return;
	}
	
		public function test_nlz():Void 
	{
		eq(32, BigIntHelper.nlz(0));
		eq(31, BigIntHelper.nlz(1));
		eq(30, BigIntHelper.nlz(2));
		eq(30, BigIntHelper.nlz(3));
		eq(29, BigIntHelper.nlz(4));
		eq(29, BigIntHelper.nlz(5));
		eq(29, BigIntHelper.nlz(7));
		eq(28, BigIntHelper.nlz(8));
		eq(28, BigIntHelper.nlz(9));
		eq(28, BigIntHelper.nlz(15));
		eq(27, BigIntHelper.nlz(16));
		eq(27, BigIntHelper.nlz(17));
		eq(27, BigIntHelper.nlz(31));
		eq(26, BigIntHelper.nlz(32));
		eq(26, BigIntHelper.nlz(33));
		eq(26, BigIntHelper.nlz(63));
		eq(25, BigIntHelper.nlz(64));
		eq(25, BigIntHelper.nlz(65));
		eq(25, BigIntHelper.nlz(127));
		eq(24, BigIntHelper.nlz(128));
		eq(24, BigIntHelper.nlz(129));
		eq(24, BigIntHelper.nlz(255));
		eq(23, BigIntHelper.nlz(256));
		eq(23, BigIntHelper.nlz(257));
		eq(23, BigIntHelper.nlz(511));
		eq(22, BigIntHelper.nlz(512));
		eq(22, BigIntHelper.nlz(513));
		eq(22, BigIntHelper.nlz(1023));
		eq(21, BigIntHelper.nlz(1024));
		eq(21, BigIntHelper.nlz(1025));
		eq(21, BigIntHelper.nlz(2047));
		eq(20, BigIntHelper.nlz(2048));
		eq(20, BigIntHelper.nlz(2049));
		eq(20, BigIntHelper.nlz(4095));
		eq(19, BigIntHelper.nlz(4096));
		eq(19, BigIntHelper.nlz(4097));
		eq(19, BigIntHelper.nlz(8191));
		eq(18, BigIntHelper.nlz(8192));
		eq(18, BigIntHelper.nlz(8193));
		eq(18, BigIntHelper.nlz(16383));
		eq(17, BigIntHelper.nlz(16384));
		eq(17, BigIntHelper.nlz(16385));
		eq(17, BigIntHelper.nlz(32767));
		eq(16, BigIntHelper.nlz(32768));
		eq(16, BigIntHelper.nlz(32769));
		eq(16, BigIntHelper.nlz(65535));
		eq(15, BigIntHelper.nlz(65536));
		eq(15, BigIntHelper.nlz(65537));
		eq(15, BigIntHelper.nlz(131071));
		eq(14, BigIntHelper.nlz(131072));
		eq(14, BigIntHelper.nlz(131073));
		eq(14, BigIntHelper.nlz(262143));
		eq(13, BigIntHelper.nlz(262144));
		eq(13, BigIntHelper.nlz(262145));
		eq(13, BigIntHelper.nlz(524287));
		eq(12, BigIntHelper.nlz(524288));
		eq(12, BigIntHelper.nlz(524289));
		eq(12, BigIntHelper.nlz(1048575));
		eq(11, BigIntHelper.nlz(1048576));
		eq(11, BigIntHelper.nlz(1048577));
		eq(11, BigIntHelper.nlz(2097151));
		eq(10, BigIntHelper.nlz(2097152));
		eq(10, BigIntHelper.nlz(2097153));
		eq(10, BigIntHelper.nlz(4194303));
		eq(9, BigIntHelper.nlz(4194304));
		eq(9, BigIntHelper.nlz(4194305));
		eq(9, BigIntHelper.nlz(8388607));
		eq(8, BigIntHelper.nlz(8388608));
		eq(8, BigIntHelper.nlz(8388609));
		eq(8, BigIntHelper.nlz(16777215));
		eq(7, BigIntHelper.nlz(16777216));
		eq(7, BigIntHelper.nlz(16777217));
		eq(7, BigIntHelper.nlz(33554431));
		eq(6, BigIntHelper.nlz(33554432));
		eq(6, BigIntHelper.nlz(33554433));
		eq(6, BigIntHelper.nlz(67108863));
		eq(5, BigIntHelper.nlz(67108864));
		eq(5, BigIntHelper.nlz(67108865));
		eq(5, BigIntHelper.nlz(134217727));
		eq(4, BigIntHelper.nlz(134217728));
		eq(4, BigIntHelper.nlz(134217729));
		eq(4, BigIntHelper.nlz(268435455));
		eq(3, BigIntHelper.nlz(268435456));
		eq(3, BigIntHelper.nlz(268435457));
		eq(3, BigIntHelper.nlz(536870911));
		eq(2, BigIntHelper.nlz(536870912));
		eq(2, BigIntHelper.nlz(536870913));
		eq(2, BigIntHelper.nlz(1073741823));
		eq(1, BigIntHelper.nlz(1073741824));
		eq(1, BigIntHelper.nlz(1073741825));
		eq(1, BigIntHelper.nlz(2147483647));
		eq(0, BigIntHelper.nlz(-2147483648));
		eq(0, BigIntHelper.nlz(-2147483647));
		eq(0, BigIntHelper.nlz(-1));
	}

	public function test_ntz():Void 
	{
		eq(32, BigIntHelper.ntz(0));
		eq(31, BigIntHelper.ntz(-2147483648));
		eq(30, BigIntHelper.ntz(1073741824));
		eq(29, BigIntHelper.ntz(536870912));
		eq(28, BigIntHelper.ntz(268435456));
		eq(27, BigIntHelper.ntz(134217728));
		eq(26, BigIntHelper.ntz(67108864));
		eq(25, BigIntHelper.ntz(33554432));
		eq(24, BigIntHelper.ntz(16777216));
		eq(23, BigIntHelper.ntz(8388608));
		eq(22, BigIntHelper.ntz(4194304));
		eq(21, BigIntHelper.ntz(2097152));
		eq(20, BigIntHelper.ntz(1048576));
		eq(19, BigIntHelper.ntz(524288));
		eq(18, BigIntHelper.ntz(262144));
		eq(17, BigIntHelper.ntz(131072));
		eq(16, BigIntHelper.ntz(65536));
		eq(15, BigIntHelper.ntz(32768));
		eq(14, BigIntHelper.ntz(16384));
		eq(13, BigIntHelper.ntz(8192));
		eq(12, BigIntHelper.ntz(4096));
		eq(11, BigIntHelper.ntz(2048));
		eq(10, BigIntHelper.ntz(1024));
		eq(9, BigIntHelper.ntz(512));
		eq(8, BigIntHelper.ntz(256));
		eq(7, BigIntHelper.ntz(128));
		eq(6, BigIntHelper.ntz(64));
		eq(5, BigIntHelper.ntz(32));
		eq(4, BigIntHelper.ntz(16));
		eq(3, BigIntHelper.ntz(8));
		eq(2, BigIntHelper.ntz(4));
		eq(1, BigIntHelper.ntz(2));
		eq(0, BigIntHelper.ntz(-2147483647));
		eq(0, BigIntHelper.ntz(-1));
		eq(0, BigIntHelper.ntz(1));
		eq(0, BigIntHelper.ntz(3));
	}

	public function test_clp2():Void 
	{
		eq(0, BigIntHelper.clp2(0));
		eq(1, BigIntHelper.clp2(1));
		eq(2, BigIntHelper.clp2(2));
		eq(4, BigIntHelper.clp2(3));
		eq(4, BigIntHelper.clp2(4));
		eq(8, BigIntHelper.clp2(5));
		eq(8, BigIntHelper.clp2(7));
		eq(8, BigIntHelper.clp2(8));
		eq(16, BigIntHelper.clp2(9));
		eq(16, BigIntHelper.clp2(15));
		eq(16, BigIntHelper.clp2(16));
		eq(32, BigIntHelper.clp2(17));
		eq(32, BigIntHelper.clp2(31));
		eq(32, BigIntHelper.clp2(32));
		eq(64, BigIntHelper.clp2(33));
		eq(64, BigIntHelper.clp2(63));
		eq(64, BigIntHelper.clp2(64));
		eq(128, BigIntHelper.clp2(65));
		eq(128, BigIntHelper.clp2(127));
		eq(128, BigIntHelper.clp2(128));
		eq(256, BigIntHelper.clp2(129));
		eq(256, BigIntHelper.clp2(255));
		eq(256, BigIntHelper.clp2(256));
		eq(512, BigIntHelper.clp2(257));
		eq(512, BigIntHelper.clp2(511));
		eq(512, BigIntHelper.clp2(512));
		eq(1024, BigIntHelper.clp2(513));
		eq(1024, BigIntHelper.clp2(1023));
		eq(1024, BigIntHelper.clp2(1024));
		eq(2048, BigIntHelper.clp2(1025));
		eq(2048, BigIntHelper.clp2(2047));
		eq(2048, BigIntHelper.clp2(2048));
		eq(4096, BigIntHelper.clp2(2049));
		eq(4096, BigIntHelper.clp2(4095));
		eq(4096, BigIntHelper.clp2(4096));
		eq(8192, BigIntHelper.clp2(4097));
		eq(8192, BigIntHelper.clp2(8191));
		eq(8192, BigIntHelper.clp2(8192));
		eq(16384, BigIntHelper.clp2(8193));
		eq(16384, BigIntHelper.clp2(16383));
		eq(16384, BigIntHelper.clp2(16384));
		eq(32768, BigIntHelper.clp2(16385));
		eq(32768, BigIntHelper.clp2(32767));
		eq(32768, BigIntHelper.clp2(32768));
		eq(65536, BigIntHelper.clp2(32769));
		eq(65536, BigIntHelper.clp2(65535));
		eq(65536, BigIntHelper.clp2(65536));
		eq(131072, BigIntHelper.clp2(65537));
		eq(131072, BigIntHelper.clp2(131071));
		eq(131072, BigIntHelper.clp2(131072));
		eq(262144, BigIntHelper.clp2(131073));
		eq(262144, BigIntHelper.clp2(262143));
		eq(262144, BigIntHelper.clp2(262144));
		eq(524288, BigIntHelper.clp2(262145));
		eq(524288, BigIntHelper.clp2(524287));
		eq(524288, BigIntHelper.clp2(524288));
		eq(1048576, BigIntHelper.clp2(524289));
		eq(1048576, BigIntHelper.clp2(1048575));
		eq(1048576, BigIntHelper.clp2(1048576));
		eq(2097152, BigIntHelper.clp2(1048577));
		eq(2097152, BigIntHelper.clp2(2097151));
		eq(2097152, BigIntHelper.clp2(2097152));
		eq(4194304, BigIntHelper.clp2(2097153));
		eq(4194304, BigIntHelper.clp2(4194303));
		eq(4194304, BigIntHelper.clp2(4194304));
		eq(8388608, BigIntHelper.clp2(4194305));
		eq(8388608, BigIntHelper.clp2(8388607));
		eq(8388608, BigIntHelper.clp2(8388608));
		eq(16777216, BigIntHelper.clp2(8388609));
		eq(16777216, BigIntHelper.clp2(16777215));
		eq(16777216, BigIntHelper.clp2(16777216));
		eq(33554432, BigIntHelper.clp2(16777217));
		eq(33554432, BigIntHelper.clp2(33554431));
		eq(33554432, BigIntHelper.clp2(33554432));
		eq(67108864, BigIntHelper.clp2(33554433));
		eq(67108864, BigIntHelper.clp2(67108863));
		eq(67108864, BigIntHelper.clp2(67108864));
		eq(134217728, BigIntHelper.clp2(67108865));
		eq(134217728, BigIntHelper.clp2(134217727));
		eq(134217728, BigIntHelper.clp2(134217728));
		eq(268435456, BigIntHelper.clp2(134217729));
		eq(268435456, BigIntHelper.clp2(268435455));
		eq(268435456, BigIntHelper.clp2(268435456));
		eq(536870912, BigIntHelper.clp2(268435457));
		eq(536870912, BigIntHelper.clp2(536870911));
		eq(536870912, BigIntHelper.clp2(536870912));
		eq(1073741824, BigIntHelper.clp2(536870913));
		eq(1073741824, BigIntHelper.clp2(1073741823));
		eq(1073741824, BigIntHelper.clp2(1073741824));
		eq(-2147483648, BigIntHelper.clp2(1073741825));
		eq(-2147483648, BigIntHelper.clp2(2147483647));
		eq(-2147483648, BigIntHelper.clp2(-2147483648));
		eq(0, BigIntHelper.clp2(-2147483647));
		eq(0, BigIntHelper.clp2(-1));
	}

	public function test_u32gtu32():Void 
	{
		checkUnsignedRelation(0, 0);
		checkUnsignedRelation(1, 0);
		checkUnsignedRelation(2147483647, 2147483647);
		checkUnsignedRelation(-2147483648, 2147483647);
		checkUnsignedRelation(-2147483648, -2147483648);
		checkUnsignedRelation(-2147483647, -2147483648);
		checkUnsignedRelation(-1, -1);
		checkUnsignedRelation(-1, -2);
	}

	private function checkUnsignedRelation(a:Int, b:Int):Void 
	{
		eq(a != b, BigIntHelper.u32gtu32(a, b));
		f(BigIntHelper.u32gtu32(b, a));
		t(BigIntHelper.u32geu32(a, b));
		eq(a == b, BigIntHelper.u32geu32(b, a));
	}

	public function testDivision():Void 
		{
		divide1(0, 1, 0, 0);
		divide1(0, 2147483647, 0, 0);
		divide1(0, -2147483648, 0, 0);
		divide1(0, -1, 0, 0);

		divide1(1, 1, 1, 0);
		divide1(1, 2, 0, 1);
		divide1(1, 2147483647, 0, 1);
		divide1(1, -2147483648, 0, 1);
		divide1(1, -1, 0, 1);

		divide1(-1, 2147483647, 2, 1);
		divide1(-2, 2147483647, 2, 0);
		divide1(-3, 2147483647, 1, 2147483646);
		divide1(-4, 2147483647, 1, 2147483645);
		divide1(-2147483648, 2147483647, 1, 1);
		divide1(2147483647, 2147483647, 1, 0);
		divide1(2147483646, 2147483647, 0, 2147483646);
		divide1(2147483645, 2147483647, 0, 2147483645);

		divide1(-1, -2147483648, 1, 2147483647);
		divide1(-2, -2147483648, 1, 2147483646);
		divide1(-3, -2147483648, 1, 2147483645);
		divide1(-2147483646, -2147483648, 1, 2);
		divide1(-2147483647, -2147483648, 1, 1);
		divide1(-2147483648, -2147483648, 1, 0);
		divide1(2147483647, -2147483648, 0, 2147483647);
		divide1(2147483646, -2147483648, 0, 2147483646);
		divide1(2147483645, -2147483648, 0, 2147483645);

		divide1(0, -2147483647, 0, 0);
		divide1(2147483647, -2147483647, 0, 2147483647);
		divide1(-2147483648, -2147483647, 0, -2147483648);
		divide1(-2147483647, -2147483647, 1, 0);
		divide1(-2147483646, -2147483647, 1, 1);
		divide1(-2147483645, -2147483647, 1, 2);

		divide1(4500, 501, 8, 492);

		divide1(-1, 1, -1, 0);
		divide1(-1, 2, 2147483647, 1);
		divide1(-1, 3, 1431655765, 0);
		divide1(-1, 4, 1073741823, 3);
		divide1(-1, 5, 858993459, 0);
		divide1(-1, 6, 715827882, 3);
		divide1(-1, 7, 613566756, 3);
		divide1(-1, 8, 536870911, 7);
		divide1(-1, 9, 477218588, 3);
		divide1(-1, 10, 429496729, 5);
		divide1(-1, 11, 390451572, 3);

		divide1(-2147483648, 1, -2147483648, 0);
		divide1(-2147483648, 2, 1073741824, 0);
		divide1(-2147483648, 3, 715827882, 2);
		divide1(-2147483648, 4, 536870912, 0);
		divide1(-2147483648, 5, 429496729, 3);
		divide1(-2147483648, 6, 357913941, 2);
		divide1(-2147483648, 7, 306783378, 2);
		divide1(-2147483648, 8, 268435456, 0);
		divide1(-2147483648, 9, 238609294, 2);
		divide1(-2147483648, 10, 214748364, 8);
		divide1(-2147483648, 11, 195225786, 2);

		divide1(0, 65535, 0, 0);
		divide1(1, 65535, 0, 1);
		divide1(65534, 65535, 0, 65534);
		divide1(65535, 65535, 1, 0);
		divide1(65536, 65535, 1, 1);
		divide1(2147483647, 65535, 32768, 32767);
		divide1(-2147483648, 65535, 32768, 32768);
		divide1(-2, 65535, 65536, 65534);
		divide1(-1, 65535, 65537, 0);
	}

	private function divide1(dividend:Int, divisor:Int, expectedQuotient:Int, expectedRemainder:Int):Void {
		if ((0 < divisor) && (divisor < 65536)) {
			eq(expectedQuotient, BigIntHelper.u32divu16(dividend, divisor));
		}
	}

	public function testAbs():Void
	{
		var an:BigInt = -1;
		var am:MutableBigInt = -1;
		eq("1",an.abs().toString());
		eq("1",am.abs().toString());
		an = -2147483648;
		am = -2147483648;
		eq("2147483648",an.abs().toString());
		eq("2147483648",am.abs().toString());
		an = 0;
		am = 0;
		eq("0",an.abs().toString());
		eq("0",am.abs().toString());
		am = 2147483647;
		an = 2147483647;
		eq("2147483647",an.abs().toString());
		eq("2147483647",am.abs().toString());
	}

	public function testPow():Void 
	{
		var b:BigInt = 2;
		var bm:MutableBigInt = 2;
		for (i in 0...(s_powersOfTwo.length-3)) {
			var s = s_powersOfTwo[i];
			eq(s,b.pow(i).toString());
			eq(s,bm.pow(i).toString());
		}
	}

	public function testModPow():Void
	{
		var b:BigInt = 2;
		var bm:MutableBigInt = 2;
		var exp:BigInt = 3;
		var mod:BigInt = 5;
		eq("3",b.modPow(exp,mod).toString());
		eq("3",b.modPow(exp,mod).toString());
		exp = 5;
		mod = 13;
		eq("6",b.modPow(exp,mod).toString());
		eq("6",b.modPow(exp,mod).toString());
	}

	public function testPrimeNumber():Void
	{
		for(i in 0...s_primeNumbers.length) {
			var b:BigInt = s_primeNumbers[i];
			var bm:MutableBigInt = s_primeNumbers[i];
			t(b.isProbablePrime(10));
			t(bm.isProbablePrime(10));
		}
		for(i in 0...s_notPrimeNumbers.length) {
			var b:BigInt = s_notPrimeNumbers[i];
			var bm:MutableBigInt = s_notPrimeNumbers[i];
			f(b.isProbablePrime(10));
			f(bm.isProbablePrime(10));
		}
	}

	public function testLowestSetBit():Void
	{
		var b:BigInt = 2162;
		var bm:MutableBigInt = 2162;
		eq(1, b.getLowestSetBit());
		eq(1, bm.getLowestSetBit());
		b = 5607;
		bm = 5607;
		eq(0, b.getLowestSetBit());
		eq(0, bm.getLowestSetBit());
		b = 3520;
		bm = 3520;
		eq(6, b.getLowestSetBit());
		eq(6, bm.getLowestSetBit());
		b = 2068;
		bm = 2068;
		eq(2, b.getLowestSetBit());
		eq(2, bm.getLowestSetBit());
		b = 4583952;
		bm = 4583952;
		eq(4, b.getLowestSetBit());
		eq(4, bm.getLowestSetBit());
	}

	public function testBitLength():Void
	{
		var b:BigInt = 4036232;
		var bm:MutableBigInt = 6661810;
		eq(22, b.bitLength());
		eq(23, bm.bitLength());
		b = 54342;
		bm = 4471;
		eq(16, b.bitLength());
		eq(13, bm.bitLength());
		b = 241;
		bm = 519;
		eq(8, b.bitLength());
		eq(10, bm.bitLength());
	}
	
	public function testGcd():Void 
	{
		var gcdCache = new Array<Array<Int>>();
		var a:BigInt ,b:BigInt;
		for(i in 0...255) {
			gcdCache[i] = new Array<Int>();
			var j = i;
			a = i;
			while ( j >=0 ) {
				b = j;
				gcdCache[i][j] = gcdCache[j][i] = Std.parseInt(a.gcd(b).toString());
				j--;
			}
		}
		for(i in 0...255) {
			eq(i,gcdCache[i][0]);
			eq(1,gcdCache[i][1]);
		}
		a = 259;
		b = 261;
		eq("1",a.gcd(b).toString());
		a = 42; 
		b = -7;
		eq("7",b.gcd(a).toString());
		a = 255*3;
		b = 255*5;
		eq("255",a.gcd(b).toString());
		a = 128; // 256/2
		b = 64;  // 256/4
		eq("64",a.gcd(b).toString());
		a = 6;
		b = 3;
		eq("3",a.gcd(b).toString());
	}
	
	public function testLcm():Void
	{
		var a:BigInt ,b:BigInt;
		a = 12; b = 15;
		eq("60",a.lcm(b).toString());
		a = 30; b = 25;
		eq("150",a.lcm(b).toString());
		a = 26; b = 20;
		eq("260",a.lcm(b).toString());
		a = "234516789234023485693020129"; b = "176892058718950472893785940";
		eq("41484157651764614525905399263631111992263435437186260",a.lcm(b).toString());
		a = "36594652830916364940473625749407"; b = "448507083624364748494746353648484939";
		eq("443593541011902763984944550799004089258248037004507648321189937329",a.lcm(b).toString());
	}

	public function testMaxMin():Void
	{
		var a:BigInt ,b:BigInt;
		a = -1; b = 23;
		eq("-1",a.min(b).toString());
		eq("-1",b.min(a).toString());
		a = 2399; b= 692421;
		eq("692421",b.max(a).toString());
		eq("692421",a.max(b).toString());
	}
	
	public function testNextProbablePrime():Void
	{
		var a:BigInt;
		a = "8329132432461";
		eq("8329132432469",a.nextProbablePrime().toString());
		a = 269234;
		eq("269237",a.nextProbablePrime().toString());
		a = 409993;
		eq("409999",a.nextProbablePrime().toString());
		a = 950091;
		eq("950099",a.nextProbablePrime().toString());
		a = 141682;
		eq("141689",a.nextProbablePrime().toString());
		a = 40870716;
		eq("40870721",a.nextProbablePrime().toString());
		a = 32747015;
		eq("32747023",a.nextProbablePrime().toString());
	}
	
	public function testBigIntRandom():Void
	{
		for(j  in 3...15) {
			for ( i in 0...100) {
				var random = BigInt.random(Std.parseInt(s_powersOfTwo[j+2]));
				eq(Std.parseInt(s_powersOfTwo[j]),random.toHex().length);
			}
		}
	}
	
	public function testBigIntRandomPrime():Void
	{
		var randomPrimeNumber = BigInt.randomPrime(5,5);
		t(randomPrimeNumber.isProbablePrime(5));
		randomPrimeNumber = BigInt.randomPrime(11,5);
		t(randomPrimeNumber.isProbablePrime(5));
		randomPrimeNumber = BigInt.randomPrime(16,5);
		t(randomPrimeNumber.isProbablePrime(5));
		randomPrimeNumber = BigInt.randomPrime(32,5);
		t(randomPrimeNumber.isProbablePrime(5));
		randomPrimeNumber = BigInt.randomPrime(55,5);
		t(randomPrimeNumber.isProbablePrime(5));
		randomPrimeNumber = BigInt.randomPrime(128,5);
		t(randomPrimeNumber.isProbablePrime(5));
		randomPrimeNumber = BigInt.randomPrime(156,5);
		t(randomPrimeNumber.isProbablePrime(5));
	}

	public function testBigIntRandomInRange():Void
	{
		var a:BigInt = "100";
		var b:BigInt = 1000;
		var randomBigInt = BigInt.randomInRange(a,b);
		t( (randomBigInt>=a && randomBigInt<=b) );
		a = "100000";
		b = "1000000";
		randomBigInt = BigInt.randomInRange(a,b);
		t( (randomBigInt>=a && randomBigInt<=b) );
		a = 1;
		b = "151115727451828646838272";
		randomBigInt = BigInt.randomInRange(a,b);
		t( (randomBigInt>=a && randomBigInt<=b) );
		a = "618970019642690137449562112";
		b = "604462909807314587353088";
		try {
			randomBigInt = BigInt.randomInRange(a,b);
		} catch (e) {
			eq(cast BigIntExceptions.INVALID_ARGUMENT, e.message);
		}
	}
	
	public function testModInverse():Void
	{
		var m:Array<String> = [	"2885628006", "3452672361", "2693781441",  "3446368347", "1495928706" , "3144152002", "1680946273","-9223372036854775808","-8192","-2147483648"];
		var n:Array<String> = [ "898595849", "2540385371", "1304452195", "2448267533", "2053023521", "4287024253", "1920144361",	"504475217", "887965291", "300193913", "2394418165" ];
		var mn:Array<String> = ["681936597","980871030","323007506","112883568","683798641","1331447622","1514136460","438360889","585723972","102755466","818276521",
		"565144203","2230262406","55288816","1361792736","293899217","244543810","1508822196","237344825","670834209","151186306","1741057836",
		"198144249","2077049753","1187239881","2435102952","1947607550","2483570286","725766574","284956416","606548574","242581937","1287460391",
		"500716917","910017586","666455683","1268170143","751080606","3325520905","1230560432","124770931","580887745","121788521","1422101743",
		"858074026","791210169","823419966","1678016129","1682774644","1196897481","244305979","462331726","867267019","166516939","1303195691",
		"712131840","2106472611","967422418","273950480","1682853299","4248168436","1597159081","254025827","233894683","63539412","736767788",
		"279559525","387786944","1228135902","494650428","146720719","1425245481","1438770027","120084998","310304596","244283899","1212900932",
		"493541265","1302212172","774141982","2347206816","1101394049","2159509590","1432890654","292669872","867212529","222172747","2347398892",
		"386883247","331502925","1088688923","170649507","200239965","2567923341","699193192","83689187","270226742","133350299","1768630898",
		"530941007","522306373","1263806667","20481954","1522248974","3285124637","954156434","247829081","734364965","178328821","1058287987"];
		
		var pos = 0;
		for(i in 0...m.length) {
			var a:BigInt = BigInt.fromString(m[i]);
			for(j in 0...n.length) {
				var b:BigInt = BigInt.fromString(n[j]);
				var r = a.modInverse(b);
				eq(mn[pos],r.toString());
				pos++;
			}
		}
		
		// Negative base test
		var a:BigInt = BigInt.fromString("-1000000007");
		var b:BigInt = BigInt.fromString("4294967296");
		var r = a.modInverse(b);
		eq("2226617417",r.toString());
	}

	public function testBitCount():Void
	{
		var b:BigInt;
		for(i in 0...s_primeNumbers.length) 
		{
			b = s_primeNumbers[i];
			eq(bitcount_primeNumbers[i],b.bitCount());
		}
		b = "31289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095312899111185499677385095379916592335598588061783255927420803548214727728870207630191983948724376068401899627616123563545172204899055530302352272847778610639873706336353202093761877582737778967644388283980069216329164759974622415192924361423912834834097982572847229665289965486725933953557514921726179978395828682342961308195443724242730929928110631592986930117802707120080197203803352168090966852137865755500415252722786337003493152656342665044813201880867313011779208419714474015014448780182032616958398612905151589972853856670263969276968800487107824408674583370084303094968400391314759922362195986830202207596680332502343698371432493225860868672103795917753114605230060644403093508418935416904505089602088327325601412709663730602066474016981743423981229695359364504322373458499999084429970918315257406576639170288335216159597192504909615825763191413466758905361986953760342352846455046778423348196835629206819558975114241389728719748889950094477184342553768772940642849290238015121220983265906214423712649581203267665136745074202516467260825331767062907475837419769957088063227358080456524599640705221357017747930954881301847074664566735498598935477860222597799333432825231323448244500921120637415399726170951129421035380206210953128991111854996773850953799165923355985880617832559274208035482147277288702076301919839487243760684018996276161235635451722048990555303023522728477786106398737063363532020937618775827377789676443882839800692163291647599746224151929243614239128348340979825728472296652899654867259339535575149217261799783958286823429613081954437242427309299281106315929869301178027071200801972038033521680909668521378657555004152527227863370034931526563426650448132018808673130117792084197144740150144487801820326169583986129051515899728538566702639692769688004871078244086745833700843030949684003913147599223621959868302022075966803325023436983714324932258608686721037959177531146052300606444030935084189354169045050896020883273256014127096637306020664740169817434239812296953593645043223734584999990844299709183152574065766391702883352161595971925049096158257631914134667589053619869537603423528464550467784233481968356292068195589751142413897287197488899500944771843425537687729406428492902380151212209832659062144237126495812032676651367450742025164672608253317670629074758374197699570880632273580804565245996407052213570177479309548813018470746645667354985989354778602225977993334328252313234482445009211206374153997261709511294210353802062109531289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095312899111185499677385095379916592335598588061783255927420803548214727728870207630191983948724376068401899627616123563545172204899055530302352272847778610639873706336353202093761877582737778967644388283980069216329164759974622415192924361423912834834097982572847229665289965486725933953557514921726179978395828682342961308195443724242730929928110631592986930117802707120080197203803352168090966852137865755500415252722786337003493152656342665044813201880867313011779208419714474015014448780182032616958398612905151589972853856670263969276968800487107824408674583370084303094968400391314759922362195986830202207596680332502343698371432493225860868672103795917753114605230060644403093508418935416904505089602088327325601412709663730602066474016981743423981229695359364504322373458499999084429970918315257406576639170288335216159597192504909615825763191413466758905361986953760342352846455046778423348196835629206819558975114241389728719748889950094477184342553768772940642849290238015121220983265906214423712649581203267665136745074202516467260825331767062907475837419769957088063227358080456524599640705221357017747930954881301847074664566735498598935477860222597799333432825231323448244500921120637415399726170951129421035380206210953128991111854996773850953799165923355985880617832559274208035482147277288702076301919839487243760684018996276161235635451722048990555303023522728477786106398737063363532020937618775827377789676443882839800692163291647599746224151929243614239128348340979825728472296652899654867259339535575149217261799783958286823429613081954437242427309299281106315929869301178027071200801972038033521680909668521378657555004152527227863370034931526563426650448132018808673130117792084197144740150144487801820326169583986129051515899728538566702639692769688004871078244086745833700843030949684003913147599223621959868302022075966803325023436983714324932258608686721037959177531146052300606444030935084189354169045050896020883273256014127096637306020664740169817434239812296953593645043223734584999990844299709183152574065766391702883352161595971925049096158257631914134667589053619869537603423528464550467784233481968356292068195589751142413897287197488899500944771843425537687729406428492902380151212209832659062144237126495812032676651367450742025164672608253317670629074758374197699570880632273580804565245996407052213570177479309548813018470746645667354985989354778602225977993334328252313234482445009211206374153997261709511294210353802062109531289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095";
		eq(14286,b.bitCount());
	}

	public function testIsPositive():Void
	{
		var b:BigInt = -1;
		f(b.isPositive());
		b = 0;
		t(b.isPositive());
		b = 1;
		t(b.isPositive());
		b = 3829;
		t(b.isPositive());
		b = -32943;
		f(b.isPositive());
	}
	
	public function testEvenOdd():Void
	{
		var b:BigInt = 1;
		t(b.isOdd());
		f(b.isEven());
		b = 2;
		f(b.isOdd());
		t(b.isEven());
		b = 76;
		f(b.isOdd());
		t(b.isEven());
		b = "500638577896799483816152607866547736957740898853209483873285677143564366192332208307174317959001709443039404185797701672275527838488848483763636556445777023797930138165123350019004132380446348231021254368110746126663615959395864308678978278260535734556772116555567464463944778761494325692023874761887965433325891748738093112709958788369487884977010548779088188484331392128315526085363468945546963420585208800664404356458139205589044250148264071701123009387700818846733471543158424023118048291252187133437780648242543956566170672422350843150080779372519053879333392134884951949440626103615875779513578928323532154688532003749917394291989161377389875366073468404983368368097031044949613470296667047208143363341323720962260335461968963306358427170789478369967512574983206915797533599998535087953469304411850522622672461336345855355508007855385321221106261546814248579179126016547764554328074845477357114937006730911294360182786223565951598223920151163494948086030036705028558864380824193953573225449943077940239329925228264218792118724026347617320530827300651961339871631931340901163772928730439359425128354171228396689527810082955319463306776797758296764576356156478933492520370117517191201473793019864639561873521807073656608329937521";
		t(b.isOdd());
		f(b.isEven());
	}

	public function testTestBit():Void
	{
		var b:BigInt = "31289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095312899111185499677385095379916592335598588061783255927420803548214727728870207630191983948724376068401899627616123563545172204899055530302352272847778610639873706336353202093761877582737778967644388283980069216329164759974622415192924361423912834834097982572847229665289965486725933953557514921726179978395828682342961308195443724242730929928110631592986930117802707120080197203803352168090966852137865755500415252722786337003493152656342665044813201880867313011779208419714474015014448780182032616958398612905151589972853856670263969276968800487107824408674583370084303094968400391314759922362195986830202207596680332502343698371432493225860868672103795917753114605230060644403093508418935416904505089602088327325601412709663730602066474016981743423981229695359364504322373458499999084429970918315257406576639170288335216159597192504909615825763191413466758905361986953760342352846455046778423348196835629206819558975114241389728719748889950094477184342553768772940642849290238015121220983265906214423712649581203267665136745074202516467260825331767062907475837419769957088063227358080456524599640705221357017747930954881301847074664566735498598935477860222597799333432825231323448244500921120637415399726170951129421035380206210953128991111854996773850953799165923355985880617832559274208035482147277288702076301919839487243760684018996276161235635451722048990555303023522728477786106398737063363532020937618775827377789676443882839800692163291647599746224151929243614239128348340979825728472296652899654867259339535575149217261799783958286823429613081954437242427309299281106315929869301178027071200801972038033521680909668521378657555004152527227863370034931526563426650448132018808673130117792084197144740150144487801820326169583986129051515899728538566702639692769688004871078244086745833700843030949684003913147599223621959868302022075966803325023436983714324932258608686721037959177531146052300606444030935084189354169045050896020883273256014127096637306020664740169817434239812296953593645043223734584999990844299709183152574065766391702883352161595971925049096158257631914134667589053619869537603423528464550467784233481968356292068195589751142413897287197488899500944771843425537687729406428492902380151212209832659062144237126495812032676651367450742025164672608253317670629074758374197699570880632273580804565245996407052213570177479309548813018470746645667354985989354778602225977993334328252313234482445009211206374153997261709511294210353802062109531289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095312899111185499677385095379916592335598588061783255927420803548214727728870207630191983948724376068401899627616123563545172204899055530302352272847778610639873706336353202093761877582737778967644388283980069216329164759974622415192924361423912834834097982572847229665289965486725933953557514921726179978395828682342961308195443724242730929928110631592986930117802707120080197203803352168090966852137865755500415252722786337003493152656342665044813201880867313011779208419714474015014448780182032616958398612905151589972853856670263969276968800487107824408674583370084303094968400391314759922362195986830202207596680332502343698371432493225860868672103795917753114605230060644403093508418935416904505089602088327325601412709663730602066474016981743423981229695359364504322373458499999084429970918315257406576639170288335216159597192504909615825763191413466758905361986953760342352846455046778423348196835629206819558975114241389728719748889950094477184342553768772940642849290238015121220983265906214423712649581203267665136745074202516467260825331767062907475837419769957088063227358080456524599640705221357017747930954881301847074664566735498598935477860222597799333432825231323448244500921120637415399726170951129421035380206210953128991111854996773850953799165923355985880617832559274208035482147277288702076301919839487243760684018996276161235635451722048990555303023522728477786106398737063363532020937618775827377789676443882839800692163291647599746224151929243614239128348340979825728472296652899654867259339535575149217261799783958286823429613081954437242427309299281106315929869301178027071200801972038033521680909668521378657555004152527227863370034931526563426650448132018808673130117792084197144740150144487801820326169583986129051515899728538566702639692769688004871078244086745833700843030949684003913147599223621959868302022075966803325023436983714324932258608686721037959177531146052300606444030935084189354169045050896020883273256014127096637306020664740169817434239812296953593645043223734584999990844299709183152574065766391702883352161595971925049096158257631914134667589053619869537603423528464550467784233481968356292068195589751142413897287197488899500944771843425537687729406428492902380151212209832659062144237126495812032676651367450742025164672608253317670629074758374197699570880632273580804565245996407052213570177479309548813018470746645667354985989354778602225977993334328252313234482445009211206374153997261709511294210353802062109531289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095";
		t(b.testBit(1));
		f(b.testBit(10));
		t(b.testBit(200));
		f(b.testBit(12345));
		t(b.testBit(12347));
		var nb:BigInt = "-57406576";
		t(nb.testBit(350));
		nb = "57406576";
		f(nb.testBit(350));
	}
	
	public function testSetBit():Void
	{
		var b:BigInt = 0;
		eq("1024",b.setBit(10).toString());
		b = "51929243614239128348340";
		eq("51929243614239136736948",b.setBit(23).toString());
		b = "-39972617095";
		eq("-39972617095",b.setBit(68).toString());
	}
	
	public function testClearBit():Void
	{
		var b:BigInt = 0;
		eq("0",b.clearBit(10).toString());
		b = "51929243614239128348340";
		eq("51929243614236980864692",b.clearBit(31).toString());
		b = "-39972617095";
		eq("-295147905219325442951",b.clearBit(68).toString());
	}
	
	public function testFlipBit():Void
	{
		var b:BigInt ="-4611686018427387904";
		eq("-4611686016279904256",b.flipBit(31).toString());
		b = "-4611686018427387904";
		eq("-13835058055282163712",b.flipBit(63).toString());
		b = "-9223372036854775808";
		eq("-18446744073709551616",b.flipBit(63).toString());
	}
	
	public function testGetPowerOfTwo():Void
	{
		for (i in 0...96) {
			eq(s_powersOfTwo[i], BigInt.getPowerOfTwo(i).toString());
		}
		eq(s_powersOfTwo[96], BigInt.getPowerOfTwo(128).toString());
		eq(s_powersOfTwo[97], BigInt.getPowerOfTwo(256).toString());
		eq(s_powersOfTwo[98], BigInt.getPowerOfTwo(512).toString());
	}
	
	public function testStringConversion():Void
	{
		var b:BigInt ="-4611686018427387904";
		eq("-4611686018427387904",b);
		eq("-46116860184273879041",(b+"1"));
		b = "9876543210";
		eq("29876543210",("2"+b));
	}

	public function testDivMod():Void
	{
		var b:BigInt = BigInt.random(48);
		var result = BigInt.divMod(b,b);
		eq("1",result.quotient);
		eq("0",result.remainder);
		result = BigInt.divMod(b,1);
		eq(b.toString(),result.quotient);
		eq("0",result.remainder);
		for(i in 0...10) {
			var a:BigInt = BigInt.random(100-i).abs();
			var b:BigInt = BigInt.random(100+i).abs();
			var c:BigInt = BigInt.random(10+i).abs();
			var d:BigInt = (a*b)+c;
			var result = BigInt.divMod(d,a);
			eq(result.quotient.toString(),b.toString());
			eq(result.remainder.toString(),c.toString());
		}
	}
	
	public function testHashCode():Void
	{
		var b:BigInt = "248576108700009";
		eq(845966795,b.hashCode());
		b = "101096178448040";
		eq(-269403574,b.hashCode());
		b = "1194923254814264634767542811261";
		eq(1465720594,b.hashCode());
		b = "2568876284867254520019857975684381983699779161674564743775191726214883120720241149248282662729370472124792889942528657175";
		eq(965626026,b.hashCode());
	}
	
	public function testBitwiseOperations():Void
	{
		var a:BigInt = "-4611686016279904256";
		var b:BigInt = "4611686023796097025";
		var result = a & b;
		eq("4611686018427387904",result.toString());
		result = a | b;
		eq("-4611686010911195135",result.toString());
		result = a ^ b;
		eq("-9223372029338583039",result.toString());
		result = ~a;
		eq("4611686016279904255",result.toString());
		result = ~b;
		eq("-4611686023796097026",result.toString());
	}
	
	public function testSquare():Void 
	{
		var b:BigInt = "31289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095312899111185499677385095379916592335598588061783255927420803548214727728870207630191983948724376068401899627616123563545172204899055530302352272847778610639873706336353202093761877582737778967644388283980069216329164759974622415192924361423912834834097982572847229665289965486725933953557514921726179978395828682342961308195443724242730929928110631592986930117802707120080197203803352168090966852137865755500415252722786337003493152656342665044813201880867313011779208419714474015014448780182032616958398612905151589972853856670263969276968800487107824408674583370084303094968400391314759922362195986830202207596680332502343698371432493225860868672103795917753114605230060644403093508418935416904505089602088327325601412709663730602066474016981743423981229695359364504322373458499999084429970918315257406576639170288335216159597192504909615825763191413466758905361986953760342352846455046778423348196835629206819558975114241389728719748889950094477184342553768772940642849290238015121220983265906214423712649581203267665136745074202516467260825331767062907475837419769957088063227358080456524599640705221357017747930954881301847074664566735498598935477860222597799333432825231323448244500921120637415399726170951129421035380206210953128991111854996773850953799165923355985880617832559274208035482147277288702076301919839487243760684018996276161235635451722048990555303023522728477786106398737063363532020937618775827377789676443882839800692163291647599746224151929243614239128348340979825728472296652899654867259339535575149217261799783958286823429613081954437242427309299281106315929869301178027071200801972038033521680909668521378657555004152527227863370034931526563426650448132018808673130117792084197144740150144487801820326169583986129051515899728538566702639692769688004871078244086745833700843030949684003913147599223621959868302022075966803325023436983714324932258608686721037959177531146052300606444030935084189354169045050896020883273256014127096637306020664740169817434239812296953593645043223734584999990844299709183152574065766391702883352161595971925049096158257631914134667589053619869537603423528464550467784233481968356292068195589751142413897287197488899500944771843425537687729406428492902380151212209832659062144237126495812032676651367450742025164672608253317670629074758374197699570880632273580804565245996407052213570177479309548813018470746645667354985989354778602225977993334328252313234482445009211206374153997261709511294210353802062109531289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095312899111185499677385095379916592335598588061783255927420803548214727728870207630191983948724376068401899627616123563545172204899055530302352272847778610639873706336353202093761877582737778967644388283980069216329164759974622415192924361423912834834097982572847229665289965486725933953557514921726179978395828682342961308195443724242730929928110631592986930117802707120080197203803352168090966852137865755500415252722786337003493152656342665044813201880867313011779208419714474015014448780182032616958398612905151589972853856670263969276968800487107824408674583370084303094968400391314759922362195986830202207596680332502343698371432493225860868672103795917753114605230060644403093508418935416904505089602088327325601412709663730602066474016981743423981229695359364504322373458499999084429970918315257406576639170288335216159597192504909615825763191413466758905361986953760342352846455046778423348196835629206819558975114241389728719748889950094477184342553768772940642849290238015121220983265906214423712649581203267665136745074202516467260825331767062907475837419769957088063227358080456524599640705221357017747930954881301847074664566735498598935477860222597799333432825231323448244500921120637415399726170951129421035380206210953128991111854996773850953799165923355985880617832559274208035482147277288702076301919839487243760684018996276161235635451722048990555303023522728477786106398737063363532020937618775827377789676443882839800692163291647599746224151929243614239128348340979825728472296652899654867259339535575149217261799783958286823429613081954437242427309299281106315929869301178027071200801972038033521680909668521378657555004152527227863370034931526563426650448132018808673130117792084197144740150144487801820326169583986129051515899728538566702639692769688004871078244086745833700843030949684003913147599223621959868302022075966803325023436983714324932258608686721037959177531146052300606444030935084189354169045050896020883273256014127096637306020664740169817434239812296953593645043223734584999990844299709183152574065766391702883352161595971925049096158257631914134667589053619869537603423528464550467784233481968356292068195589751142413897287197488899500944771843425537687729406428492902380151212209832659062144237126495812032676651367450742025164672608253317670629074758374197699570880632273580804565245996407052213570177479309548813018470746645667354985989354778602225977993334328252313234482445009211206374153997261709511294210353802062109531289911118549967738509537991659233559858806178325592742080354821472772887020763019198394872437606840189962761612356354517220489905553030235227284777861063987370633635320209376187758273777896764438828398006921632916475997462241519292436142391283483409798257284722966528996548672593395355751492172617997839582868234296130819544372424273092992811063159298693011780270712008019720380335216809096685213786575550041525272278633700349315265634266504481320188086731301177920841971447401501444878018203261695839861290515158997285385667026396927696880048710782440867458337008430309496840039131475992236219598683020220759668033250234369837143249322586086867210379591775311460523006064440309350841893541690450508960208832732560141270966373060206647401698174342398122969535936450432237345849999908442997091831525740657663917028833521615959719250490961582576319141346675890536198695376034235284645504677842334819683562920681955897511424138972871974888995009447718434255376877294064284929023801512122098326590621442371264958120326766513674507420251646726082533176706290747583741976995708806322735808045652459964070522135701774793095488130184707466456673549859893547786022259779933343282523132344824450092112063741539972617095112942103538020621095";
		eq(b.square().toString(),"979058537806756893235764283621924582421624967959196705564083760865282878022528107541866696789999935394715055870410306002387950402245568340192774553939964784785136787035734012929020031542885375787843603153559735941700261460304443075567543513333520762338421517475867224096445639606155592889669086721727997857312141851700193120492947139298141762261650624989441203684454733571273858218678419781871095287063187170220305598627305996931173474871683260238740358756331777477446041053891726564542483445966219729545222131712519817549673448935818892604478829972483257523220664889053686981485569952646163209531357683066245563257049257290320527044324067319043073011746430085797237805622069664157003517740615901207267842299601544397898380578066816137990306245652655168526632019958283225460517476594435652706609647029870409172355571128872501060775377848615950844966107944014008828433520438618661476482804692649631032580865675177913085826911130919185912284560057852718730476885523564039401019893536044992969889829486583733742979641766664498732850142086385975404324354631901442327591309984820899557314159969342026806890391805464777141402390942224468818370286189339496153617050553815039679012522805912033547445688105403239187775875234695574708938333630275401964101884672755647448401504106167065029833455420760044079588321937865328594849576565652107016941893441134441489443804881782134985154316330370659296182735277532522634915945354671652745525615738151340897705499294856121970124424262561578588661070768748543272426858577119516700312630657114436724547836855591208750226685060140040174125537545428385497463753943072571227529783378333550538113917364144160466355547190632269349246026760183016106138179385808844467631134147595861974359080407426949021054600827406974588925698075948850086018471308981465533181474754519822334992687302851294717402499224703819753452353952665382241366283063603874724557289535418803460892960913805996252420050456490043065470303385155367575762141894594926816357914610921880140706035292094245838934453097365588075603899819378216402761593808343896644462707142751099198293773175651073305368077604244261547846445304405563072251272763052240231016311108836221199803715720948798172146146822837581513933599860625531522416596165538352143985260629484252013240603746124859559804801379978198335907753720463429955164228727856745536104065928159895022176223424186676545666373418638624515974026952078389390603147781211534420797830979303858034305405963664641808204943737765697507602185501362004131536520605837623881178803800749437858792505505238150955053761142804861634042146799366363235647799188637302595418142869067049152019189445176191871946879297026005090278766372933530402711461980515815870976408958180928496880238421145181532819117861814761897826373390693324175380062318156951380609989834514389153506089552696574682402647450330958493005951193468281744803168784091237754176890610088913252782656385227417856173153796739713680301256174874938347811850314525153900194383799119062025926320526532206551632108138480165731746313415303742768329957794485047908468899083433663540940626762422522727602719098293608567645087759241483407201426209279494495557718081985308778026799719285340918877391360803048053840729645544493187813037548603853178682920950366515895564102068512725726599625068190958931320820002004036777484199104280379016410671705960669241533187095062758439830792180062352002958237102534913594033310904155723190519957657651063409063077922268057110521276843832626321784094221331838291720863598362201637482982557498487167128503528010841650137600621194618825718913787878637874158719897046754343494436690880586535743871385530923679862732430558987722978407824753264841508350145784200495705312694541403861480587124503513617051615355165667276602067006869573031643215099845702145930825314542363371139576115632234293137920599511834828365563730149358865909350003317859285250309685685322448925566338410052906109034239095149839766387933915982151176143681446909556796553781528029993609380244575797782603417796157573908355342053078921667619277056117773641377739105214399260243775552861901116387297062708321870433018669121765566223545992426192779945681105161465320976335025140676734385352196650808854595037261192773876921157777828951638074323437757154167756183736779714987927379914616922390432227884248509928423348348430559903215059285516157829243864992514282729100714602148178096293798530554736397701129911531687838676479862011069639846771617321911275098402664072736901473534364233835113885008174723233809864532594170861087659797792026735593810021274574062256230262763106103582405624406237587116610638356059536376119763753940718949067160997044098102742891715519335938406585897503407052001156147872384634992909881846670028110702885992070643183658137173695169489605344750236969441460956409016907469503728703722147166164667184895723312744451221079250974388851014976113215778389415709550864976338414714415282198899781199485143413547642073070333703453041991729851722204831893949661284948278952019511080557920516117663492394801410785471153142305358908430005926122088425456777703581015061172890411482084864438241187387006519018519275910817740324420828827644510756798743746171580678747205430023940914349021412922348648813690711415108878616582735938385145246351411703000579449900946625771746517557897313864023240171779661756642902980917766773849558999536597964746250315579594570258217076171836485913926745555332472687917200213209151831145639385355204417097626516952963946980108265813591932269507107101560962958228173321459342668360139766838080290225100612874548521822048912950051246755516443085776878657865951322820940997926521055322919755491720427953784844995505352482256716630626962365260947518734500150983169085369416384607241048573938054469346018079223454714934100062708452711675796747743061334696567353621054901052045563971433032825936923138111509516160074782990668727851860537550871639285515258390953003025377043760348813217411905320008316595565458523978429030228547755053027452998995175647639943390182154386587745054114420276029928192002183064155459505927825924761579454674977915931429123445912383711223199802040947961400754470453505830317493844788975124448221382310191952533682912781122146683082391476343476658971305372339703264307994088812368699633302945684255186725732149566605376689208649708557952071235063928208521921638706696956978895222709161811838140625659720965483125965649466305620990898284638221976728990986110673596024748603068820173274453290277719992553394376988087147536192699854703696831257004809823152987020172366759024877411215679846667947979625629206158992947430092382748449689237292162545063528868619202132002813473146939690670565728963371542027328203377969064564838223281496044438357883928062929973366523657541287665770728437725161128159572548336754811659677422647914446119472330449409152185942362464109300247247566241601010329150568050984755339721417405906913032262120848538170347935377515114091440534423595745973234353951904308633432470661288766005933908325052008189916021474966380674134932827403820468308320796110487662800348838804901944857674627156276552711225120265741479650321111046607622511421133615885466219848951124866271064232623319161702832620320985812565769534282355893996230694166186143435849907269168064034810754986647503829851613467173289068392573651838028922900314648377450393531143583816750062902886157099248004141517455567598675078129986917659553974623072881996532017303312951423269948489427346337474567050931279551592114178104089724635607643426578772606556619200412455441264533191560785328613363654343177058053149391625925452695753785715482584334910884922112645277220436430224875036739943591075606371659859276719819380485592830769141538584119994664043511989409613945923228426088712826794015991635045433619288407984613715948778173531046347421269068661500735906738223958994942051180343962226786386225079562960188735451247381713929691525021691293258352247725982625532038939581534112770333526190756331097202065372516619762994729457492597236608568955727177474282927067210683487165503436740184555313983058368167202940704498139158356192549926106790359255487403131245256305322759776145691881722537735295644854065755734058815021899821147778542096447711944544555831969873184076358578694692304910423228482342315118981773770275896509680560104263150802420378954735614559800229314380102608494927015762720221236167177065286299871146478614024094441318121290539962911275851233224992849873863853472487662792342374621868583424205303953003806365145842037440594321508048994039450143247474952305522443977985981848347500051216277479959331743762886935403161780865286690273664306622918530850353874293567047639319408911445092697967750333603130724172641250428389449056562149439457772098451500193607687895623323255051560597700216364754287016049618308265945915598140295299428864792348279417890496335999764799723377295880178138388616282678807101505063159043076029435424924434449983438966508307518404837610082324185448409740983426274119333595029404167113115571345781544510075027421837066648483090981306751619907006574990840580184247733926044068290871205886535700964967693443291378082148156471961404731219743349886722482388399453312965849803842174078285120894482557766035736617890804563860933609721423985565220653852601113466810124437810418123628866832608093735106338339281681197686377580906991909819386947372192208193807792914011052186195542268703604906783161391718242369214363051953508772528764914098124209883134840160988974375082206875752429698422869961586201438865137227466498780344632268877616455836955028195745432199012214566093788426734896427602881130895700657925091137063876782576805529321712228712412046942787237773316735567582106081198124968018422759524327594862650007755577313899017799238889835939210900477636509864421205366316945887920718051392657115460441707108460847476464658496375184681429018860147478916472622133059471957807878706723582473881788629449293192131183141381383255100303848368743038674886424218569631350744012169601301656150040411667906186126466240213067358779275164956727625356201271124037634481582421614520901026344285906407258219144968485453766872962047170767508345105097312558473235381855130609229850363238554647115223277709842816658505498263271711216973228823465919263339412938833019142547692355216394005235960151895301946938138439525275147705396962178617518897046756909784178570788086123644897401235813633836627013994252949644707204764909465367988926276571669599785516750087883455403706951252608219016723917761980351131178716191835706810278807060769342879988454378752614885205636350107221016905706593519588364424326380293124926984152797373115619712333464649984809290962210178283440940851583744148883240779537809363479701668431277094535940063256409207851734008457011686022301457866460799062788837512278476351191385837723684726370499480334642976528441236439506451335480344792804657735849831865698036435024341188410923606416113500691009420488030502017683305884129287111772392113118105299771643657823455593913888807935224033551492753626825190866319437505229406469526933182854232259117138420794309592767975765661091563926101109446375546249808276056926376765844205394328009108309263349755210400976963238022469204558553782707161547476995519474602463700797495652173588102086053896102656385344758908707574738477594082536001517861689859382656818044046913380398557146569060508080807456749090437099445859299158403758726019709293205961353845166781010084709885920713222757990542698031905931283922753264912183807112337251273918199144288998795229749429557212337248539854704750365732110349701110156451761724370838641426339242277953507657250751884243150702397551607636820152948812798713208923142370697019343943739933731428095880027617752931691192237935921833174221762512227642824270355163763778328703596790395576941218369535992958364386329638923149931697030069265708225083207138577837327908085523886178614683557013995007282129363623466154422006486255003169767873785577934471188033670031226022158341870038521548996347206723282507451181518028888988599985831521008142211253793305953380227850186040926064440677039456643814386742386767038738956761028557477615582563524193404544938340263430199152490227856864275406633309475961577261586548264042971376545222894683855068656717020043412951999872122737596678299448863678992619486373799097595121950036554336891372411335816530630374926714204977800384733267855177069495929023615252846972148680966069694029234375612509787303587011721373986404835988275089143850334909002304971423108431298031820971017250657655822007016261523607626593785638945491684270778303839017330417109466713313070772447685604024912166351472658343918673070647678606166153918299296346229486196086493470878145003410811791454556757742415897084215912803731595062068631010740382955588067548231555440602400281163362114692223482506911173482597712097137548366988480261540780525297561281650664288330753311108094505535497773231926048521312729711914820122953553129255011984363508563022857046044887286400137126681237301836422247394860497137647809066100949105902199295532277004491087892339281981764942881526472351650073141466820850148506490225828672659350803924624758928375237930065714836775985441576539363588522580632963947030514117525916615597030134225775532235504362377550454298896020771868995301024260452707393482017656523539423165677165169590143999328253255567269567189795948650767506049877532429391507449287416736167661934246754355368954919719737478341992333919920435303695061748558898502288220579611937045718983677314923368119989815466036626627159071442052756040188596921300127461029871660607174219997387589646544931026867059664465864805498041039504146108516638643800353782888363710003275290216361963711324980397167818041536506926948614828740618253563236350370780885869122282358966292565822218552380089543792971568003611161385442306752627532428215453334703278803036788298741099537326502500859442024251527187693849307825916118735329658547141763845798413045235962742879521826723303607022349504509421458191314928136136818395243397383958062535148230669082809499539122578293479517158078608333415541809880049885430762801317943046011454302540146595229430084143746206733356552659843922409085928252997097477911977002141233347328714428305826491310860463609692928571519377072393277141764324860281090386614809101456857865531792469932291931059706938712287605045603626194417741186655105097485616577764282242205158143159113648110019391854743408713977004493855911937487680134684145308088447784880789972381836502432857283089021555438796753919670987928768539045015107685769859220936859324098934322183626437106714639922023296912494373235992392235410808531254500697010218769686064321692670853731418619243688952239753879669604097316655680577378904726567038910038378346311255088640128479375846164643919480203675211044259743422347115531460192078561831336910259121510400840498867866503799589160367918428557637473117904779400106254843836638814527073968538102089100967459757533321815753505481396344315222760421597204233520615657379881597125149690292010284942412311964882191645145397324944402010839701892977759616820807500333274758186843480695658149881775717169927138485156226883699426086984521595747462415627179725573799181816194944757650549166616930682222300725224524427177561150044792135101010674848277170186967642531760448521330571951858411831611988171446919656085499898193794491396010719692122454035051905551731529814999780225686966169423209747016448359803189186522381241969799837476516113549085542010216346690901491734100761928676402295031587802742954328688048521401921200469344333041491539080744786765390438873504174710519234683777251162823168951265639070175701478522789292937214237920687831813821844046702190941091244602065942275038246224086293810902100231890829031589035548290993713304631432698536501361454867851020669522964286510847203185407336079460871073173136859732094601769065425696348340323438181969045850896957925102654624550674545652803034063022575771477223761318756036570147073628869199194732764960803588040913260630162482459868228616866335176316929564526216675510519204738466423465553523707290651363629405302657554264620738138610694231207966746808040361775963012457734565670649521309478368862282664377414766444012242629852108309047592323726235369396045873737223618350460839291860000487032541791807954657364954040795038009858866088269702986665289626915881262912911691311918245824000318193755082376648206724577200772863732702289987879709348371716222822650281097388162702300195372997674929869383476556104306213959982142476495458402896722951141078659723229307403290685582440933404578633717832081585226774784139827789557758029696660009922964340338529347082479871369640619219404866083516324290106524553918947673139895664479235034076666092127442687524397095559872522717393378176428075183874859177016157693554859136910386135230747446158028309375544874183926983365480035154804796967087602664759453846673269943887016056186890845687185275805353410361232474067012685927243542225974756276328914425087901035573852775473215774749469219258652111434926727458049392297998581419997475257597015091772213299871042652811840227660801516736767741762412310585769891416679950629089122017832085293293491105697878907168637230258746237581573546149263529790352058912880680596138857701176997051232900729184103879737378314302354200961078302557160030390095926240622208147137114293449558999025");
		b = -1;
		eq("1",b.square().toString());
		b = 12;
		eq("144",b.square().toString());
		b = "900638577896799483816152607866547736957740898853209483873285677143564366192332208307174317959001709443039404185797701672275527838488848483763636556445777023797930138165123350019004132380446348231021254368110746126663615959395864308678978278260535734556772116555567464463944778761494325692023874761887965433325891748738093112709958788369487884977010548779088188484331392128315526085363468945546963420585208800664404356458139205589044250148264071701123009387700818846733471543158424023118048291252187133437780648242543956566170672422350843150080779372519053879333392134884951949440626103615875779513578928323532154688532003749917394291989161377389875366073468404983368368097031044949613470296667047208143363341323720962260335461968963306358427170789478369967512574983206915797533599998535087953469304411850522622672461336345855355508007855385321221106261546814248579179126016547764554328074845477357114937006730911294360182786223565951598223920151163494948086030036705028558864380824193953573225449943077940239329925228264218792118724026347617320530827300651961339871631931340901163772928730439359425128354171228396689527810082955319463306776797758296764576356156478933492520370117517191201473793019864639561873521807073656608329937521";
		eq(b.square().toString(),"811149847995969351721277742900450882666128710880121943723033984496363909727632962176457328745441351015478577651463199674431737573765944282100259530965252603943339128013246587325032433979335734786504965901799889302406159701354628874288473762021809902804053601718275980939845906748371292333374386010272739798132621713025723929014161498355914599120590999020567498930685525496898528572272450620596571129956342956107921718415101699685615809685762171982015939351781590111612963744323021219017314395169101957513801384312440238545652940865450311026811203970970957029411203919505705426812806790770118405250890709523784587944630212866255970356938290335586926983865860825950787572716874669983883676778931308475575258301756972135670253797560275576412260135518662419116827857095886038355919354007003597455667513169127242846260995278068044555964903013553940393196332871118985123422282045524588981442057877700191236290306997574581238117918228368072872123983495741091953470906723396416933752597404582681058872156303027788029666728474877486709304615595192903559931696626288338307760680901186871217690767936503046402666623639181700299821260147573719588148214702677548426987049866959796951843501932327233549325130570874940881569441505740992412152163422139000345033086150490637830379626663095530609392696422973033783031490813002844626281482749160143146002262443245511657909938799353706943265245247971607714172276190975033083092323436499204422426985912227333265456839641376409257331923063349532372142400057625690712436017349830479014752049237163321310012828072244966656829228823891878508108627558804036584962958423530832782619333442944482366365076764192311978676820924849805425113873330915805633022353818149007166650579284997431056643960362882638106719229415840167611751425985456191915329684820329204462418286776934054555573154503205065497338435727750784376231689637383309663963162154399631027567945337979218900759282873105744106028948285791212405440632741973882786929715605978665500815612346882365153383022113445334714770937230143310380914232402858039429871518297820436033846958308390362520620180048952725186450999823465158343616672346992013811084929170401354704051849364793860083975422550113785138190483582776827060329176948827042813576191758992791063707390435752053126595798631052684617709069724195377659960922914914394808920839148428685275090182476586835424154247611667167737411950171729176182187724122365481161419269660486776280489015370727450596846878409032710715406436303763625441");
	}

	private static var bitcount_primeNumbers = [1,2,2,3,3,3,2,3,4,4,5,3,3,4,5,4,5,5,3,4,3,5,4,4,3,4,5,5,5,4,7,3,3,4,4,5,5,4,5,5,5,5,7,3,4,5,5,7,5,5,5,7,5,7,2,4,4,5,4,4,5,4,5,6,5,6,5,4,6,6,4,6,7,6,7,8,4,5,4,5,5,5,7,5,7,7,4,5,6,7,6,8,7,7,7,8,8,3,4,5,4,5,5,5,6,3,5,4,6,5,7,5,5,6,7,3,4,5,5,5,5,4,5,6,6,7,5,7,7,7,6,7,8,7,7,3,4,5,6,5,6,6,7,7,7,6,6,6,7,8,7,6,7,8,6,7,7,5,6,7,7,7,7,7,6,8,9,7];

	
	private static var s_primeNumbers = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997];
	
	private static var s_notPrimeNumbers = [0,1,4,6,8,9,10,12,14,15,16,18,20,21,22,24,25,26,27,28,30,32,33,34,35,36,38,39,40,42,44,45,46,48,49,50,51,52,54,55,56,57,58,60,62,63,64,65,66,68,69,70,72,74,75,76,77,78,80,81,82,84,85,86,87,88,90,91,92,93,94,95,96,98,99,100,102,104,105,106,108,110,111,112,114,115,116,117,118,119,120,121,122,123,124,125,126,128,129,130,132,133,134,135,136,138,140,141,142,143,144,145,146,147,148,150,152,153,154,155,156,158,159,160,161,162,164,165,166,168,169,170,171,172,174,175,176,177,178,180,182,183,184,185,186,187,188,189,190,192,194,195,196,198,200,201,202,203,204,205,206,207,208,209,210,212,213,214,215,216,217,218,219,220,221,222,224,225,226,228,230,231,232,234,235,236,237,238,240,242,243,244,245,246,247,248,249,250,252,253,254,255,256,258,259,260,261,262,264,265,266,267,268,270,272,273,274,275,276,278,279,280,282,284,285,286,287,288,289,290,291,292,294,295,296,297,298,299,300,301,302,303,304,305,306,308,309,310,312,314,315,316,318,319,320,321,322,323,324,325,326,327,328,329,330,332,333,334,335,336,338,339,340,341,342,343,344,345,346,348,350,351,352,354,355,356,357,358,360,361,362,363,364,365,366,368,369,370,371,372,374,375,376,377,378,380,381,382,384,385,386,387,388,390,391,392,393,394,395,396,398,399,400,402,403,404,405,406,407,408,410,411,412,413,414,415,416,417,418,420,422,423,424,425,426,427,428,429,430,432,434,435,436,437,438,440,441,442,444,445,446,447,448,450,451,452,453,454,455,456,458,459,460,462,464,465,466,468,469,470,471,472,473,474,475,476,477,478,480,481,482,483,484,485,486,488,489,490,492,493,494,495,496,497,498,500,501,502,504,505,506,507,508,510,511,512,513,514,515,516,517,518,519,520,522,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,542,543,544,545,546,548,549,550,551,552,553,554,555,556,558,559,560,561,562,564,565,566,567,568,570,572,573,574,575,576,578,579,580,581,582,583,584,585,586,588,589,590,591,592,594,595,596,597,598,600,602,603,604,605,606,608,609,610,611,612,614,615,616,618,620,621,622,623,624,625,626,627,628,629,630,632,633,634,635,636,637,638,639,640,642,644,645,646,648,649,650,651,652,654,655,656,657,658,660,662,663,664,665,666,667,668,669,670,671,672,674,675,676,678,679,680,681,682,684,685,686,687,688,689,690,692,693,694,695,696,697,698,699,700,702,703,704,705,706,707,708,710,711,712,713,714,715,716,717,718,720,721,722,723,724,725,726,728,729,730,731,732,734,735,736,737,738,740,741,742,744,745,746,747,748,749,750,752,753,754,755,756,758,759,760,762,763,764,765,766,767,768,770,771,772,774,775,776,777,778,779,780,781,782,783,784,785,786,788,789,790,791,792,793,794,795,796,798,799,800,801,802,803,804,805,806,807,808,810,812,813,814,815,816,817,818,819,820,822,824,825,826,828,830,831,832,833,834,835,836,837,838,840,841,842,843,844,845,846,847,848,849,850,851,852,854,855,856,858,860,861,862,864,865,866,867,868,869,870,871,872,873,874,875,876,878,879,880,882,884,885,886,888,889,890,891,892,893,894,895,896,897,898,899,900,901,902,903,904,905,906,908,909,910,912,913,914,915,916,917,918,920,921,922,923,924,925,926,927,928,930,931,932,933,934,935,936,938,939,940,942,943,944,945,946,948,949,950,951,952,954,955,956,957,958,959,960,961,962,963,964,965,966,968,969,970,972,973,974,975,976,978,979,980,981,982,984,985,986,987,988,989,990,992,993,994,995,996,998,999];
	

	private static var s_powersOfTwo = [
		"1",
		"2",
		"4",
		"8",
		"16",
		"32",
		"64",
		"128",
		"256",
		"512",
		"1024",
		"2048",
		"4096",
		"8192",
		"16384",
		"32768",
		"65536",
		"131072",
		"262144",
		"524288",
		"1048576",
		"2097152",
		"4194304",
		"8388608",
		"16777216",
		"33554432",
		"67108864",
		"134217728",
		"268435456",
		"536870912",
		"1073741824",
		"2147483648",
		"4294967296",
		"8589934592",
		"17179869184",
		"34359738368",
		"68719476736",
		"137438953472",
		"274877906944",
		"549755813888",
		"1099511627776",
		"2199023255552",
		"4398046511104",
		"8796093022208",
		"17592186044416",
		"35184372088832",
		"70368744177664",
		"140737488355328",
		"281474976710656",
		"562949953421312",
		"1125899906842624",
		"2251799813685248",
		"4503599627370496",
		"9007199254740992",
		"18014398509481984",
		"36028797018963968",
		"72057594037927936",
		"144115188075855872",
		"288230376151711744",
		"576460752303423488",
		"1152921504606846976",
		"2305843009213693952",
		"4611686018427387904",
		"9223372036854775808",
		"18446744073709551616",
		"36893488147419103232",
		"73786976294838206464",
		"147573952589676412928",
		"295147905179352825856",
		"590295810358705651712",
		"1180591620717411303424",
		"2361183241434822606848",
		"4722366482869645213696",
		"9444732965739290427392",
		"18889465931478580854784",
		"37778931862957161709568",
		"75557863725914323419136",
		"151115727451828646838272",
		"302231454903657293676544",
		"604462909807314587353088",
		"1208925819614629174706176",
		"2417851639229258349412352",
		"4835703278458516698824704",
		"9671406556917033397649408",
		"19342813113834066795298816",
		"38685626227668133590597632",
		"77371252455336267181195264",
		"154742504910672534362390528",
		"309485009821345068724781056",
		"618970019642690137449562112",
		"1237940039285380274899124224",
		"2475880078570760549798248448",
		"4951760157141521099596496896",
		"9903520314283042199192993792",
		"19807040628566084398385987584",
		"39614081257132168796771975168",
		/* 2^128 */ "340282366920938463463374607431768211456",
		/* 2^256 */ "115792089237316195423570985008687907853269984665640564039457584007913129639936",
		/* 2^512 */ "13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096",
	];
}