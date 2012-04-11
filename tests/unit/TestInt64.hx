package unit;
using haxe.Int64;

class TestInt64 extends Test {

	inline function eq32( i : haxe.Int32, a : Int, b : Int, ?pos : haxe.PosInfos ) {
		var i2 = haxe.Int32.make(a, b);
		if( haxe.Int32.compare(i, i2) != 0 )
			Test.report(Std.string(i)+" should be "+Std.string(i2),pos);
	}
	
	public function test() {
		eq( 1.ofInt().toInt(), 1 );
		eq( ( -1).ofInt().toInt(), -1 );
		eq( Std.string(156.ofInt()), "156" );
		
		var v = (1 << 20).ofInt();
		eq( Std.string(v), "1048576" );
		
		var p40 = v.shl(20);
		eq32( p40.getLow(), 0, 0 );
		eq32( p40.getHigh(), 0, 256 );
		eq( Std.string(p40), "1099511627776" );
		
		eq( 1.ofInt().shl(0).toStr(), "1" );
	}

}