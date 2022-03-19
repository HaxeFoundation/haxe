package unit;

using haxe.UInt64;
import haxe.UInt64.*;

class TestUInt64 extends Test {

	public function testMake() {
		var a : UInt64, i : Int;

		// Test creation and fields
		a = UInt64.make(10,0xFFFFFFFF);
		eq( a.high, 10 );
		eq( a.low, 0xFFFFFFFF );

		// Int casts
		a = 1;
		eq( a.toInt(), 1 );

		a = -1;
		eq( a.high, 0xFFFFFFFF );
		eq( a.low, 0xFFFFFFFF );
		eq( a.toInt(), -1 );

		a = UInt64.make(0,0x80000000);
		eq( a.high, 0 );
		eq( a.low, 0x80000000 );
		exc( tryOverflow.bind(a) );	// Throws Overflow

		a = UInt64.make(0xFFFFFFFF,0x80000000);
		eq( a.high, 0xFFFFFFFF );
		eq( a.low, 0x80000000 );
		eq( a.toInt(), -2147483648 );

		a = UInt64.make(0xFFFFFFFF,0x7FFFFFFF);
		eq( a.high, 0xFFFFFFFF );
		eq( a.low, 0x7FFFFFFF );
		exc( tryOverflow.bind(a) );	// Throws Overflow
	}

	function tryOverflow(a:UInt64) {
		a.toInt();
	}
		
}