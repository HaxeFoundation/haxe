package unit;

class TestBasetypes extends Test {

	function testArray() {
		var a : Array<Null<Int>> = [1,2,3];
		eq( a.length, 3 );
		eq( a[0], 1 );
		eq( a[2], 3 );

		eq( a[3], null );
		eq( a[1000], null );
		eq( a[-1], null );

		a.remove(2);
		eq( a.length, 2);
		eq( a[0], 1 );
		eq( a[1], 3 );
		eq( a[2], null );
	}

	function testString() {
		eq( String.fromCharCode(77), "M" );
		unspec(function() String.fromCharCode(0));
		unspec(function() String.fromCharCode(-1));
		unspec(function() String.fromCharCode(256));
		eq( null + "x", "nullx" );
		eq( "x" + null, "xnull" );

		var abc = "abc".split("");
		eq( abc.length, 3 );
		eq( abc[0], "a" );
		eq( abc[1], "b" );
		eq( abc[2], "c" );
	}

	function testMath() {
		eq( Std.int(-1.7), -1 );
		eq( Std.int(-1.2), -1 );
		eq( Std.int(1.7), 1 );
		eq( Std.int(1.2), 1 );
		eq( Std.int(-0.7), 0 );
		eq( Std.int(-0.2), 0 );
		eq( Std.int(0.7), 0 );
		eq( Std.int(0.2), 0 );

		eq( Math.floor(-1.7), -2 );
		eq( Math.floor(-1.5), -2 );
		eq( Math.floor(-1.2), -2 );
		eq( Math.floor(1.7), 1 );
		eq( Math.floor(1.5), 1 );
		eq( Math.floor(1.2), 1 );
		eq( Math.ceil(-1.7), -1 );
		eq( Math.ceil(-1.5), -1 );
		eq( Math.ceil(-1.2), -1 );
		eq( Math.ceil(1.7), 2 );
		eq( Math.ceil(1.5), 2 );
		eq( Math.ceil(1.2), 2 );
		eq( Math.round(-1.7), -2 );
		eq( Math.round(-1.5), -1 );
		eq( Math.round(-1.2), -1 );
		eq( Math.round(1.7), 2 );
		eq( Math.round(1.5), 2 );
		eq( Math.round(1.2), 1 );

		// overflows might occurs depending on the platform
		unspec(function() Std.int(-10000000000.7));
		unspec( function() Math.floor(-10000000000.7) );
		unspec( function() Math.ceil(-10000000000.7) );
		unspec( function() Math.round(-10000000000.7) );
		// should still give a proper result for lower bits
		eq( Std.int(-10000000000.7) & 0xFFFFFF, 15997952 );
		eq( Math.floor(-10000000000.7) & 0xFFFFFF, 15997951 );
		eq( Math.ceil(-10000000000.7) & 0xFFFFFF, 15997952 );
		eq( Math.round(-10000000000.7) & 0xFFFFFF, 15997951 );
	}

	function testParse() {
		eq( Std.parseInt("0"), 0 );
		eq( Std.parseInt("0001"), 1 );
		eq( Std.parseInt("100"), 100 );
		eq( Std.parseInt("-100"), -100 );
		eq( Std.parseInt("100x123"), 100 );
		eq( Std.parseInt(""), null );
		eq( Std.parseInt("abcd"), null );
		eq( Std.parseInt(null), null );
	}

}
