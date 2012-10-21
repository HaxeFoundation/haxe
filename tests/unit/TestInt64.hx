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

}