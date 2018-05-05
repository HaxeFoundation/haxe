package unit;

class TestNaN extends Test {

	static function foo() {
		return Math.NaN;
	}

	static function id( b : Bool ) {
		return b;
	}

	function testNanIfs() {
		var a = foo();

		if( a > 0 ) assert();
		if( a >= 0 ) assert();
		if( a < 0 ) assert();
		if( a <= 0 ) assert();
		if( a == 0 ) assert();
		if( a == a ) assert();

		if( a > 1 ) assert();
		if( a < 1 ) assert();
		if( a == 1 ) assert();

		if( a > -1 ) assert();
		if( a < -1 ) assert();
		if( a == -1 ) assert();

		if( !(a > 0) ) {} else assert();
		if( !(a >= 0) ) {} else assert();
		if( !(a < 0) ) {} else assert();
		if( !(a <= 0) ) {} else assert();
		if( !(a == 0) ) {} else assert();
		if( !(a == a) ) {} else assert();

		if( id(a > 0) ) assert();
		if( id(a >= 0) ) assert();
		if( id(a < 0) ) assert();
		if( id(a <= 0) ) assert();
		if( id(a == 0) ) assert();
		if( id(a == a) ) assert();

		if( a < 0 || a > 0 ) assert();
		if( a < 0 && a > 0 ) assert();
	}
}