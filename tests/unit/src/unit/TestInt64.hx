package unit;

using haxe.Int64;

class TestInt64 extends Test {

	public function testMake() {
		var a : Int64, i : Int;

		// Test creation and fields
		a = Int64.make(10,0xFFFFFFFF);
		eq( a.getHigh(), 10 );
		eq( a.getLow(), 0xFFFFFFFF );

		// Int casts
		a = 1;
		eq( a.toInt(), 1 );

		a = -1;
		eq( a.getHigh(), 0xFFFFFFFF );
		eq( a.getLow(), 0xFFFFFFFF );
		eq( a.toInt(), -1 );

		a = Int64.make(0,0x80000000);
		eq( a.getHigh(), 0 );
		eq( a.getLow(), 0x80000000 );
		exc( tryOverflow.bind(a) );	// Throws Overflow

		a = Int64.make(0xFFFFFFFF,0x80000000);
		eq( a.getHigh(), 0xFFFFFFFF );
		eq( a.getLow(), 0x80000000 );
		eq( a.toInt(), -2147483648 );

		a = Int64.make(0xFFFFFFFF,0x7FFFFFFF);
		eq( a.getHigh(), 0xFFFFFFFF );
		eq( a.getLow(), 0x7FFFFFFF );
		exc( tryOverflow.bind(a) );	// Throws Overflow
	}

	function tryOverflow( a : Int64 )
	{
		a.toInt();
	}

	public function testCopy() {
		var a : Int64, b : Int64;
		a = 1;
		b = a.copy();
		++b;
		f( a == b );
	}

	public function testToString() {
		var a : Int64;

		// Issue #903
		a = 0;
		eq( Std.string( a ), "0" );

		a = 156;
		eq( '$a', "156" );

		a = -1;
		eq( '$a', "-1" );

		a = Int64.make(0xFFFFFFFE,0);
		eq( a.toStr(), "-8589934592" );

		a = Int64.make(1,1);
		eq( a.toStr(), "4294967297" );
	}

	public function testComparison() {
		var a : Int64, b : Int64;
		a = 1; b = 1;
		t( a == b ); f( a != b );
		t( a <= b ); f( a < b );
		t( a >= b ); f( a > b );
		eq( a.compare(b), 0 );
		eq( a.ucompare(b), 0 );

		a = -10;
		b = -15;
		f( a == b ); t( a != b );
		f( a <= b ); f( a < b );
		t( a >= b ); t( a > b );
		t( a.compare(b) > 0 );
		t( a.ucompare(b) > 0 );

		a = Int64.make(0,0);
		b = Int64.make(1,0);
		f( a == b ); t( a != b );
		t( a <= b ); t( a < b );
		f( a >= b ); f( a > b );
		t( a.compare(b) < 0 );
		t( a.ucompare(b) < 0 );

		a = Int64.make(0x0FFFFFFF,0xFFFFFFFF);
		b = Int64.make(0x80000000,0x80000000);
		f( a == b ); t( a != b );
		f( a <= b ); f( a < b );
		t( a >= b ); t( a > b );
		t( a.compare(b) > 0 );
		t( a.ucompare(b) < 0 );

		// Issue #2317
		a = Int64.make(0xA, 0x828D97A8);
		b = 0;
		t( a.compare(b) > 0 );
		t( a.ucompare(b) > 0 );

		// Issue #2090
		a = 0;
		b = Int64.make(320, -2138504556);
		t( a.compare(b) < 0 );
		t( a.ucompare(b) < 0 );


	}

	public function testIncrement() {
		var a = Int64.make(0,0xFFFFFFFF);
		var b = a.copy();
		var c = Int64.make(1,0);
		int64eq( a++, b );
		int64eq( a--, c );
		int64eq( ++a, c );
		int64eq( --a, b );
	}

	public function testAddition() {
		var a : Int64, b : Int64;

		a = Int64.make(0,0x9E370301);
		b = Int64.make(0,0xB0590000);
		int64eq( a+b, Int64.make(0x1,0x4E900301) );

		// negation
		b = -a;
		int64eq( a+b, 0 );

		// Int64+Int
		int64eq( a+12345678, Int64.make(0,0x9EF3644F) );

		// Int+Int64
		int64eq( 0xF0002222+a, Int64.make(0,0x8E372523) );
	}

	public function testSubtraction() {
		var a : Int64, b : Int64;

		a = Int64.make(1,0);
		b = Int64.make(0,1);
		int64eq( a-b, Int64.make(0,0xFFFFFFFF) );

		b = a;
		int64eq( a-b, 0 );

		b = Int64.make(0xFFFFFFFE,1);
		int64eq( a-b, Int64.make(2,0xFFFFFFFF) );

		// Int64-Int
		int64eq( a-12345678, Int64.make(0,0xFF439EB2) );

		// Int-Int64
		int64eq( 12345678-a, Int64.make(0xFFFFFFFF,0x00BC614E) );
	}

	public function testMultiplication() {
		var a : Int64, b : Int64;

		a = Int64.make(0, 0x239B0E13);
		b = Int64.make(0, 0x39193D1B);
		int64eq( a*b, Int64.make(0x07F108C6,0x4E900301) );

		a = Int64.make(0, 0xD3F9C9F4);
		b = Int64.make(0, 0xC865C765);
		int64eq( a*b, Int64.make(0xA5EF6C6E,0x1ACD5944) );

		var a = Int64.make(0xFFF21CDA, 0x972E8BA3);
		var b = Int64.make(0x0098C29B, 0x81000001);
		int64eq( a*b, Int64.make(0xDDE8A2E8, 0xBA2E8BA3) );

		var a = Int64.make(0x81230000, 0x81230000);
		var b = Int64.make(0x90909090, 0x90909090);
		int64eq( a*b, Int64.make(0xF04C9C9C,0x53B00000) );

		var a = Int64.make(0x00001000, 0x0020020E);
		var b = Int64.make(0xBEDBEDED, 0xDEBDEBDE);
		int64eq( a*b, Int64.make(0xC45C967D,0x25FAA224) );

		// Issue #1532
		a = Int64.make(0xFFF21CDA, 0x972E8BA3);
    	b = Int64.make(0x0098C29B, 0x81000001);
    	int64eq( a*b, Int64.make(0xDDE8A2E8, 0xBA2E8BA3) );

		// Int64*Int
		a = Int64.make(0x01000000, 0x11131111);
		int64eq( a*7, Int64.make(0x07000000,0x77857777) );

		// Int*Int64
		a = Int64.make(0x91111111, 0x11111111);
		int64eq( 2*a, Int64.make(0x22222222,0x22222222) );
	}

	public function testDivision() {
		var a : Int64, b : Int64;

		a = Int64.make(0x00002342, 0xDDEF3421);
		b = Int64.make(0x00000001, 0x00002000);
		int64eq( a/b, 9026 );
		int64eq( a%b, Int64.make(0,0xD986F421) );
		var result = a.divMod(b);
		int64eq( a/b, result.quotient );
		int64eq( a%b, result.modulus );

		a = Int64.make(0xF0120AAA, 0xAAAAAAA0);
		b = Int64.make(0x00020001, 0x0FF02000);
		int64eq( a/b, -2038 );
		int64eq( a%b, Int64.make(0xFFFE131F,0x8C496AA0) );
		result = a.divMod(b);
		int64eq( a/b, result.quotient );
		int64eq( a%b, result.modulus );

		a = Int64.make(0, 2);
		b = Int64.make(0xFFFFFFFF, 0xFAFAFAFA);
		int64eq( a/b, 0 );
		int64eq( a%b, 2 );
		result = a.divMod(b);
		int64eq( a/b, result.quotient );
		int64eq( a%b, result.modulus );

		a = Int64.make(0x7ABADDAD, 0xDEADBEEF);
		b = Int64.make(0xFFFFFFF1, 0x1FFFFFFF);
		int64eq( a/b, Int64.make(0xFFFFFFFF,0xF7BFCEAE) );
		int64eq( a%b, Int64.make(0x0000000A,0x166D8D9D) );
		result = a.divMod(b);
		int64eq( a/b, result.quotient );
		int64eq( a%b, result.modulus );

		a = Int64.make(0x81234567, 0xFDECBA98);
		b = Int64.make(0xFFFFFEFF, 0xEEEEEEEE);
		int64eq( a/b, 0x007ED446 );
		int64eq( a%b, Int64.make(0xFFFFFFF5,0x31964D84) );
		result = a.divMod(b);
		int64eq( a/b, result.quotient );
		int64eq( a%b, result.modulus );

		// Int64/Int
		int64eq( a/2, Int64.make(0xC091A2B3,0xFEF65D4C) );
		int64eq( a%100, -68 );

		// Int/Int64
		int64eq( 10001/a, 0 );
		int64eq( 515151%a, 515151 );
	}

	public function testBinaryOps() {
		var a : Int64, b : Int64;

		a = haxe.Int64.make(0x0FFFFFFF, 0x00000001);
		b = haxe.Int64.make(0, 0x8FFFFFFF);
		int64eq( a&b, 1 );
		int64eq( a|b, Int64.make(0x0FFFFFFF, 0x8FFFFFFF) );
		int64eq( a^b, Int64.make(0x0FFFFFFF, 0x8FFFFFFE) );
		int64eq( ~a, Int64.make(0xF0000000, 0xFFFFFFFE) );
	}

	public function testShifts() {
		var a : Int64;

		a = 1 << 20;
		int64eq( a, 0x100000 );

		a <<= 20;
		int64eq( a, Int64.make(0x100,0x00000000) );

		a = -1;
		a >>= 4;
		int64eq( a, -1 );
		a >>>= 4;
		int64eq( a, Int64.make(0x0FFFFFFF,0xFFFFFFFF) );

		// Ensure proper overflow behavior for shift operand
		a = Int64.make(1,1);
		int64eq( a << 0, a );
		int64eq( a >> 0, a );
		int64eq( a >>> 0, a );
		int64eq( a << 64, a );
		int64eq( a >> 64, a );
		int64eq( a >>> 64, a );
	}

	/** Tests that we have all of the classic Int64 interface. */
	public function testBackwardsCompat() {
		var a : Int64 = 32.ofInt();
		var b : Int64 = (-4).ofInt();

		t( a.eq(b) );
		t( a.neq(b) );
		int64eq( a.add(b), 28 );
		int64eq( a.sub(b), 36 );
		int64eq( a.div(b), -8 );
		int64eq( a.mod(b), 0 );
		int64eq( b.shl(1), -8 );
		int64eq( b.shr(1), -2 );
		int64eq( b.ushr(1), Int64.make(0x7FFFFFFF,0xFFFFFFFE) );
		int64eq( a.and(b), 32 );
		int64eq( a.or(b), -4 );
		int64eq( a.xor(b), -36 );
		int64eq( a.neg(), -32 );
		f( a.isNeg() );  t( b.isNeg() );
		f( a.isZero() ); f( b.isZero() );
		t( a.compare(b) > 0 );
		t( a.ucompare(b) < 0 );
		int64eq( a.toInt(), 32 );
		int64eq( b.toInt(), -4 );
	}

	function int64eq( v : Int64, v2 : Int64, ?pos ) {
		Test.count++;
		if( v != v2 ) {
			Test.report(Std.string(v)+" should be "+Std.string(v2),pos);
			Test.success = false;
		}
	}
}

/*
package unit;
using haxe.Int64;
import haxe.Int64.*;

class TestInt64 extends Test {

	public function test() {
		eq( 1.ofInt().toInt(), 1 );
		eq( ( -1).ofInt().toInt(), -1 );
		eq( Std.string(156.ofInt()), "156" );

		var v = (1 << 20).ofInt();
		eq( Std.string(v), "1048576" );

		var p40 = v.shl(20);
		eq( p40.getLow(), 0 );
		eq( p40.getHigh(), 256 );
		eq( Std.string(p40), "1099511627776" );

		eq( 1.ofInt().shl(0).toStr(), "1" );

		eq(Int64.ofInt(0).toStr(), "0");
	}

	public function testCapture()
	{
		var a = Int64.make(0xFF00FF00,0xF0F0F0F0),
		    b = Int64.make(0xFF00FF00,0xF0F0F0F0);
		eq(a.compare(b), 0);
		eq(a.getHigh(), 0xFF00FF00);
		function test() return Int64.compare(a,Int64.make(0xFF00FF00,0xF0F0F0F0));
		eq(test(),0);
		function testSet(v:Int64) b = v;
		testSet( make(0xFF00FF00, 0xFF0) );
		eq(b.compare(make(0xFF00FF00,0xFF0)),0);
		eq(b.getHigh(), 0xFF00FF00);
	}

	public function testMath() {
		var a = Int64.make(0, 0x239B0E13);
		var b = Int64.make(0, 0x39193D1B);
		var c = Int64.mul(a, b);
		eq( c.toStr(), "572248275467371265" );
		// trace(Int64.toStr(c) + " should be 572248275467371265"); // but gives 7572248271172403969 in javascript

		var a = Int64.make(0, 0xD3F9C9F4);
		var b = Int64.make(0, 0xC865C765);
		var c = Int64.mul(a, b);
		eq( c.toStr(), "-6489849317865727676" );

		var a = Int64.make(0, 0x9E370301);
		var b = Int64.make(0, 0xB0590000);
		var c = Int64.add(a, b);
		eq( Int64.toStr(c), "5613028097" );

		var a = Int64.make(0xFFF21CDA, 0x972E8BA3);
		var b = Int64.make(0x0098C29B, 0x81000001);
		var c = Int64.mul(a, b);
		#if !as3
		var expected = Int64.make(0xDDE8A2E8, 0xBA2E8BA3);
		eq( expected.compare(c), 0 );
		#end
	}

	// tests ported from https://github.com/candu/node-int64-native/blob/master/test/int64.js
	public function testCompare()
	{
    var a = ofInt(2),
        b = ofInt(3);
		t(a == a);
		t(b == b);
		eq(a.compare(a), 0);
		eq(a.compare(b), -1);
		eq(b.compare(a), 1);
	}

	public function testBits()
	{
	  var x = make(0xfedcba98,0x76543210);
    var y = x.and((ofInt(0xffff))),
        z = x.or((ofInt(0xffff))),
        w = x.xor((make(0xffffffff,0xffffffff)));
    eq(y.toStr(), '12816');
    eq(z.toStr(), '-81985529216434177');
    eq(w.toStr(), '81985529216486895');
    eq(x.and(ofInt(0xffff)).toStr(), '12816');
    eq((x.or(ofInt(0xffff))).toStr(), '-81985529216434177');
    eq((x.xor(ofInt(0xffff))).toStr(), '-81985529216446993');
    eq((x.and(make(0x1,0xffffffff))).toStr(), '1985229328');
    eq((x.or(make(0x1,0xffffffff))).toStr(), '-81985522611781633');
    eq((x.xor(make(0x1, 0xffffffff))).toStr(), '-81985524597010961');
    var a = ofInt(7),
        b = a.shl(1);
    eq(b.toStr(), '14');
	}

	public function testAdd()
	{
		var a = ofInt(3),
				b = ofInt(2),
				c = make(0xffffffff,0xfffffffe);
		eq( (a.add(b)).compare(ofInt(5)), 0 );
		eq( (a.add(ofInt(4))).compare(ofInt(7)), 0 );
		eq( (c.add(ofInt(3))).compare(ofInt(1)), 0 );
		// numbers larger than int32
		eq( a.add(make(0x1, 0)).toStr(), '4294967299');
	}

	public function testNeg()
	{
		eq(Std.string(ofInt(-1)),Std.string(neg(ofInt(1))));
		eq(Std.string(ofInt(-100)),Std.string(neg(ofInt(100))));
		eq(Std.string(make(-2147483648, 1)), Std.string(neg(make(2147483647, -1)))); // -9223372036854775807 == neg(9223372036854775807)
	}
}
*/