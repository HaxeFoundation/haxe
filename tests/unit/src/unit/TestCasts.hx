package unit;

class TestCasts extends Test  {

	static function foo( f : Float ) {
		return f;
	}

	static function make( f : Dynamic ) return f;

	function testCasts() {
		var f : Int -> Float = make(foo);
		eq( f(123), 123);
	}

}