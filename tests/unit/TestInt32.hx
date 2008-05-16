package unit;
import haxe.Int32;

class TestInt32 extends Test {

	static inline function i( x ) {
		return Int32.toInt(x);
	}

	static inline function i32( x ) {
		return Int32.ofInt(x);
	}

	public function test() {
		// constants
		eq( 0xFE08BE39, -32981447 );

		// 31bits platforms might overflow on the last bit of the constant
		allow( 0x5E08BE39 >> 16, [0x5E08,0xFFFFDE08] );
		allow( 0xAE08BE39 >>> 16, [0xAE08,0x2E08] );

		var one = i32(1);
		var minone = i32(-1);
		var zero = i32(0);

		// ofInt / make
		eq( i(zero), 0 );
		eq( i(one), 1 );
		eq( i(minone), -1 );
		eq( i(i32(0x01020304)), 0x01020304 );
		eq( i(Int32.make(0x0102,0x0304)), 0x01020304 );

		// 31 bits overflow
		exc( function() i(Int32.shl(one,30)) );
		exc( function() i(Int32.shl(i32(2),30)) );
		exc( function() i(Int32.neg(Int32.add(Int32.shl(one,30),one))) );

		// check correct closure creation (not inlined)
		var f = Int32.make;
		eq( i(f(0x0102,0x0304)), 0x01020304 );

		eq( Int32.compare(one,one), 0 );
		eq( Int32.compare(one,zero), 1 );
		eq( Int32.compare(zero,one), -1 );
		eq( Int32.compare(minone,minone), 0 );
		eq( Int32.compare(minone,zero), -1 );
		eq( Int32.compare(zero,minone), 1 );

		eq( i(Int32.add(one,one)), 2 );
		eq( i(Int32.sub(minone,one)), -2 );
		eq( i(Int32.mul(i32(5),i32(100))), 500 );

		// overflow
		eq( i(Int32.mul(i32(160427),i32(160427))), 0xFE08BE39 );

		// signed divide and modulo
		eq( i(Int32.div(i32(0x3E08BE39),i32(16))), 0x03E08BE3 );
		eq( i(Int32.div(i32(0xFE08BE39),i32(16))), 0xFFE08BE4 );
		eq( i(Int32.mod(i32(0xFE08BE39),i32(0xFFFF))), -17342 );
		eq( i(Int32.mod(i32(0xE08BE39),i32(0x10000))), 0xBE39 );

		// logical
		eq( i(Int32.shl(i32(5),16)), 0x50000 );
		eq( i(Int32.shl(i32(3),30)), 0xC0000000 );
		eq( i(Int32.shr(i32(-1),16)), -1 );
		eq( i(Int32.ushr(i32(-1),16)), 0xFFFF );

		eq( i(Int32.and(i32(0xFE08BE39),i32(0xFFFF))), 0xBE39 );
		eq( i(Int32.and(i32(0xFE08BE39),i32(0xFFFF0000))), 0xFE080000 );
		eq( i(Int32.and(i32(0xFE08BE39),i32(0xFFF0000))), 0x0E080000 );

		eq( i(Int32.or(i32(0xFE08BE39),i32(0xFFFF))), 0xFE08FFFF );
		eq( i(Int32.or(i32(0xFE08BE39),i32(0xFFFF0000))), 0xFFFFBE39 );
		eq( i(Int32.or(i32(0xBE39),i32(0xFE080000))), 0xFE08BE39 );

		eq( i(Int32.xor(i32(0xFE08BE39),i32(0xCBCDEF99))), 0x35C551A0 );
		eq( i(Int32.neg(one)), -1 );
		eq( i(Int32.complement(i32(55))), -56 );
		eq( i(Int32.complement(i32(-0x10000))), 0xFFFF );
	}

}