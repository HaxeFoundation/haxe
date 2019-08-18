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

	function testInterfaces() {

		var v = new MyClass.CI1();
		eq( cast(v,MyClass.I1), v );
		exc( function() cast(v,MyClass.I2) );

		var v = new MyClass.ClassI2();
		eq( cast(v,MyClass.I1), v );
		eq( cast(v,MyClass.I2), v );

		var v = cast(v,MyClass.I2);
		eq( cast(v,MyClass.I1), v );
		exc( function() cast(v,MyClass.CovI) );

		var v : MyClass.CovI = new MyClass.Cov2();
		eq( cast(v,MyClass.CovI), v );

		var v = new MyClass.Cov3();
		eq( cast(v,MyClass.CovI), v );
		eq( cast(v,MyClass.CovI2), v );

		exc(function() { cast(new MyClass.Cov1(), MyClass.CovI); });

	}

}
