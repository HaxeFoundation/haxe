package unit;
using haxe.Int64;

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

		// FIXME: Failing unit test!
		// var a = Int64.make(0xFFF21CDA, 0x972E8BA3);
		// var b = Int64.make(0x0098C29B, 0x81000001);
		// var c = Int64.mul(a, b);
		// var expected = Int64.make(0xDDE8A2E8, 0xBA2E8BA3);
		// eq( expected.compare(c), 0 );
	}

}
