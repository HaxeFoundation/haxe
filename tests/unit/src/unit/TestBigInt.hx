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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		eq(BigInt.fromHexUnsigned("80000000 00000001 00000000").toHex(), t1.toHex());
		v1[1] = -2147483648;
		t1.setFromUnsignedInts(v1, 2);
		eq(BigInt.fromHexUnsigned("80000000 00000000").toHex(), t1.toHex());
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
		eq(BigInt.fromHexUnsigned(hex).toHex(), t.toHex());

		var sb = new StringBuf();
		var i = hex.length;
		while (i >= 2) {
			i -= 2;
			sb.addChar(hex.charCodeAt(i));
			sb.addChar(hex.charCodeAt(i + 1));
		}
		t.setFromLittleEndianBytesUnsigned(Bytes.ofHex(sb.toString()));
		eq(BigInt.fromHexUnsigned(hex).toHex(), t.toHex());
	}

	private function checkSetFromUnsignedInts(hex:String, arr:Array<Int>):Void {
		var v = Vector.fromArrayCopy(arr);
		var t:MutableBigInt = 0;
		t.setFromUnsignedInts(v, v.length);
		eq(BigInt.fromHexUnsigned(hex).toHex(), t.toHex());
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			BigIntArithmetic.multiply(a, BigInt.fromInt(2), a);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		// division by zero should throw exception
		try {
			BigInt.fromInt(1) / 0;
		} catch (e:String) {
			eq(BigIntExceptions.DIVISION_BY_ZERO, e);
		}
		try {
			BigInt.fromInt(0) / 0;
		} catch (e:String) {
			eq(BigIntExceptions.DIVISION_BY_ZERO, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		// quotient cannot be null
		try {
			BigIntArithmetic.divideInt(BigInt.fromInt(1), 10, null);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			BigIntArithmetic.divide(BigInt.fromInt(1), BigInt.fromInt(10), null, remainder);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			BigIntArithmetic.divide(dividend, divisor, quotient, remainder, divisor);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			BigIntArithmetic.divide(dividend, divisor, quotient, remainder, quotient);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			BigIntArithmetic.divide(dividend, divisor, quotient, remainder, remainder);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		eq(expectedQuotient.toHex(), quotient.toHex());

		eq(expectedQuotient.toHex(), (dividend / divisor).toHex());
		eq(expectedQuotient.toHex(), (dividendM / divisor).toHex());
		eq(expectedRemainder, (dividend % divisor));
		eq(expectedRemainder, (dividendM % divisor));

		dividendM = dividend;
		dividendM /= divisor;
		eq(expectedQuotient.toHex(), dividendM.toHex());
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
		eq(expected.toHex(), (a + b).toHex());
		eq(expected.toHex(), (am + b).toHex());
		am += b;
		eq(expected.toHex(), am.toHex());

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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			var x = BigInt.fromString("");
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			var x = BigInt.fromString("-");
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			var x = BigInt.fromString(" 0");
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			var x = BigInt.fromString("0 ");
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
			eq(a.toHex(), BigInt.fromString(s_powersOfTwo[i]).toHex());
			eq((-a).toHex(), BigInt.fromString("-" + s_powersOfTwo[i]).toHex());
			a <<= 1;
		}
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			a << -1;
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			BigInt.ONE << -1;
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			var x = BigInt.fromHex("");
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			var x = BigInt.fromHex("0q0");
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 2), fromInt(1, 2), 2, 32); // shift 32
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 1), fromInt(1, 2), 2, 1); // result too short
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 2), fromInt(1, 1), 2, 1); // input too short
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.arithmeticShiftRight(fromInt(0, 2), fromInt(1, 2), 0, 1); // length too short
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 2), fromInt(1, 2), 2, 32); // shift 32
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 1), fromInt(1, 2), 2, 1); // result too short
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 2), fromInt(1, 1), 2, 1); // input too short
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.logicalShiftRight(fromInt(0, 2), fromInt(1, 2), 0, 1); // length too short
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.subtract(fromInt(0, 2), fromInt(0, 1), fromInt(0, 2), 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.subtract(fromInt(0, 2), fromInt(0, 2), fromInt(0, 1), 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.subtract(fromInt(0, 2), fromInt(0, 2), fromInt(0, 2), 0);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.add(fromInt(0, 2), fromInt(0, 1), fromInt(0, 2), 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.add(fromInt(0, 2), fromInt(0, 2), fromInt(0, 1), 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.add(fromInt(0, 2), fromInt(0, 2), fromInt(0, 2), 0);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.setFromHexUnsigned(value, 1, "");
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.setFromHexUnsigned(null, 1, "0");
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.setFromHexUnsigned(value, 2, "0"); // buffer too small
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.setFromHexUnsigned(value, 1, "0g0"); // invalid char
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.multiplyUnsigned(result, fromInt(2), 1, result, 1);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.multiplyIntUnsigned(result, result, 1, 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		// Multiplication of same inputs is ok
		var a = fromInt(2);
		result = fromInt(0, 2);
		MultiwordArithmetic.multiplyUnsigned(result, a, 1, a, 1);
		assertEqualsBI(4, result);

		// check for result length too short
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0), fromInt(2), 1, fromInt(2), 1);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.multiplyIntUnsigned(fromInt(0), fromInt(2), 1, 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		// check input lengths
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0, 2), fromInt(2), 0, fromInt(2), 1);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0, 2), fromInt(2), 1, fromInt(2), 0);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.multiplyIntUnsigned(fromInt(0, 2), fromInt(2), 0, 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		// check input bounds
		MultiwordArithmetic.multiplyUnsigned(fromInt(0, 4), fromInt(2, 2), 2, fromInt(2, 2), 2);
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0, 4), fromInt(2, 1), 2, fromInt(2, 2), 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.multiplyUnsigned(fromInt(0, 4), fromInt(2, 2), 2, fromInt(2, 1), 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.multiplyIntUnsigned(fromInt(0, 4), fromInt(2, 1), 2, 2);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		// quotient cannot be null
		try {
			MultiwordArithmetic.divideIntUnsigned(fromInt(1), 1, 10, null, work);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.divideUnsigned(fromInt(1), 1, fromInt(10), 1, null, remainder, work);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		// work cannot be null
		try {
			MultiwordArithmetic.divideIntUnsigned(fromInt(1), 1, 10, quotient, null);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.divideUnsigned(fromInt(1), 1, fromInt(10), 1, quotient, remainder, null);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, divisor);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, quotient);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, remainder);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		// bounds
		MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, quotient, remainder, work);
		try {
			MultiwordArithmetic.divideUnsigned(dividend, 0, divisor, divisor.length, quotient, remainder, work);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, 0, quotient, remainder, work);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, divisor, divisor.length, new Vector<Int>(0), remainder, work);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
		}

		// division by zero
		try {
			MultiwordArithmetic.divideIntUnsigned(dividend, dividend.length, 0, quotient, work);
		} catch (e:String) {
			eq(BigIntExceptions.DIVISION_BY_ZERO, e);
		}
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, fromInt(0), 1, quotient, remainder, work);
		} catch (e:String) {
			eq(BigIntExceptions.DIVISION_BY_ZERO, e);
		}

		// divisor with leading 0
		try {
			MultiwordArithmetic.divideUnsigned(dividend, dividend.length, fromHex("1", 2), 2, quotient, remainder, work);
		} catch (e:String) {
			eq(BigIntExceptions.INVALID_ARGUMENT, e);
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
			var bm:BigInt = s_primeNumbers[i];
			t(b.isProbablePrime(5));
			t(bm.isProbablePrime(5));
		}
		for(i in 0...s_notPrimeNumbers.length) {
			var b:BigInt = s_notPrimeNumbers[i];
			var bm:BigInt = s_notPrimeNumbers[i];
			f(b.isProbablePrime(5));
			f(bm.isProbablePrime(5));
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
	
	public function testMacroCrash():Void 
	{
		trace("Start testMacroCrash");
		var y:BigInt = BigInt.fromHex("080000000");
		var a:BigInt = BigInt.fromInt(1);
		var x:BigInt = BigInt.fromHex("080000000");
		var b:BigInt = BigInt.fromInt(0);

		var y_b = y - b;
		if (!y_b.isZero()) {
			var q2 = y * a / y_b;
		}
		trace("End testMacroCrash");
	}
	
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