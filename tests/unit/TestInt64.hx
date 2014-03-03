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
		int64eq( a, Int64.make(0x100,00000000) );

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

		f( a.eq(b) );
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