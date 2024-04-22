package unit;

class TestOps extends Test {

	@:analyzer(ignore)
	public function testOps()
	{
		eq(1 + 2 + "", "3");
		eq((1 + 2) + "", "3");
		eq(1 + (2 + ""), "12");

		eq(4 - 3 + "", "1");
		eq((4 - 3) + "", "1");
		//eq(4 - (3 + ""), "1");

		eq(4 | 3 & 1, 1);
		eq((4 | 3) & 1, 1);
		eq(4 | (3 & 1), 5);

		eq(4 & 3 | 1, 1);
		eq((4 & 3) | 1, 1);
		eq(4 & (3 | 1), 0);

		eq( - 5 + 1, -4 );
		eq( - (5 + 1), -6 );

		t( 5 << 2 == 20 );
		t( (5 << 2) == 20 );
		t( 20 == 5 << 2 );
		t( 20 == (5 << 2) );

		eq( 5 % 3 * 4, 8 );
		eq( (5 % 3) * 4, 8 );
		eq( 5 % (3 * 4), 5 );

		eq( 20 / 2 / 2, 5 );
		eq( (20 / 2) / 2, 5 );
		eq( 20 / (2 / 2), 20 );

		eq( 2 << 3 >> 1, 8 );
		eq( (2 << 3) >> 1, 8 );
		eq( 2 << (3 >> 1), 4 );
		eq( (getA().a + 1) >> 1, 1 );

		f( (1 & 0x8000) != 0 );
		f( 1 & 0x8000 != 0 );
		f( 0 != (1 & 0x8000) );
		f( 0 != 1 & 0x8000 );

		eq( 5 * 10 % 3, 5);
		eq( 5 * (10 % 3), 5);
		eq( (5 * 10) % 3, 2);

		eq( 10 % 3 * 5, 5);
		eq( (10 % 3) * 5, 5);
		eq( 10 % (3 * 5), 10);
		eq( 100 % 100, 0);
		eq( -100 % 100, 0);
		eq( 101.5 % 100, 1.5);
		eq( -101.5 % 100, -1.5);
		var x = 101.5;
		x %= 100;
		eq( x, 1.5);
		t(Math.isNaN(5.0 % 0.0));
		t(Math.isNaN(x %= 0.0));
		#if !macro
		//t(Math.isNaN(1 % 0));
		//t(Math.isNaN(0 % 0));
		//t(Math.isNaN(x %= 0));
		#end
		var x:Dynamic = [-101.5];
		x[0] %= 100;
		eq( x[0], -1.5);
		eq( true ? 1 : 6 * 5, 1);
		eq( false ? 1 : 6 * 5, 30);
		eq( (true ? 1 : 6) * 5, 5);
		eq( (false ? 1 : 6) * 5, 30);

		eq( 1 + (5 == 6 ? 4 : 1), 2 );
		eq( 1 + 1 == 3 ? 1 : 5, 5 );

		eq( -3 == 3 ? 0 : 1, 1 );
		t( !true ? true : true );

		var k = false;
		f(k = true ? false : true);
		f(k);
		f((k = true) ? false : true);
		t(k);

		t( true || false && false );

		var x = 1;
		eq( -x++, -1);
		eq( -x--, -2);

		eq( ("bla" + "x").indexOf("x"), 3);

		eq(5 * @foo 3 + 4, 19);
		eq(5 * @foo @bar 3 + @baz 4, 19);
	}

	static function getA() return { a:1 };

	#if target.static

	@:analyzer(ignore)
	function testNullOps() {
		var a:Null<Int> = 10;
		// arithmetic
		eq(9, a - 1);
		eq(20, a * 2);
		eq(5., a / 2); // careful with Float comparison...
		eq(1, a % 3);

		// bit
		eq(20, a << 1);
		eq(5, a >> 1);
		eq(5, a >>> 1);
		eq(10, a & 15);
		eq(15, a | 15);
		eq(2, a ^ 8);

		// unary
		eq(-10, -a);
		eq(-11, ~a);

		// boolean
		var b:Null<Bool> = true;
		eq(false, !b);
		eq(false, b && falseValue);
		eq(true, b && trueValue);
		eq(true, b || falseValue);
		eq(true, b || trueValue);

		b = false;
		eq(true, !b);
		eq(false, b && falseValue);
		eq(false, b && trueValue);
		eq(false, b || falseValue);
		eq(true, b || trueValue);

		eq(true, a > 5);
		eq(true, a >= 5);
		eq(false, a < 5);
		eq(false, a <= 5);
		eq(true, a != 5);
		eq(false, a != 10);

		eq(false, 0 > a);
		eq(false, 0 >= a);
		eq(true, 0 < a);
		eq(true, 0 <= a);
		eq(true, 0 != a);
		eq(false, 0 == a);

		var minusA:Null<Int> = -10;
		eq(true, 0 > minusA);
		eq(true, 0 >= minusA);
		eq(false, 0 < minusA);
		eq(false, 0 <= minusA);
		eq(true, 0 != minusA);
		eq(false, 0 == minusA);
	}

	#if !flash // Will not fix for flash

	@:analyzer(ignore)
	function testNadakoOps() {
		// bool
		var nullBool:Null<Bool> = null;

		t(null == nullBool);
		t(nullBool == null);
		f(false == nullBool);
		f(nullBool == false);
		t(false != nullBool);
		t(nullBool != false);

		var nullBoolfalse:Null<Bool> = false;

		f(nullBoolfalse == nullBool);
		f(nullBool == nullBoolfalse);
		t(nullBoolfalse != nullBool);
		t(nullBool != nullBoolfalse);

		// int
		var nullInt:Null<Int> = null;

		t(null == nullInt);
		t(nullInt == null);
		f(null != nullInt);
		f(nullInt != null);

		f(null >  nullInt);
		f(null >= nullInt);
		f(null <  nullInt);
		f(null <= nullInt);

		f(nullInt >  null);
		f(nullInt >= null);
		f(nullInt <  null);
		f(nullInt <= null);

		f(0 == nullInt);
		f(nullInt == 0);
		t(0 != nullInt);
		t(nullInt != 0);

		f(0 > nullInt);
		f(0 >= nullInt);
		f(0 < nullInt);
		f(0 <= nullInt);

		f(nullInt > 0);
		f(nullInt >= 0);
		f(nullInt < 0);
		f(nullInt <= 0);

		f(1 > nullInt);
		f(1 >= nullInt);
		f(1 < nullInt);
		f(1 <= nullInt);

		f(nullInt > 1);
		f(nullInt >= 1);
		f(nullInt < 1);
		f(nullInt <= 1);

		f(-1 > nullInt);
		f(-1 >= nullInt);
		f(-1 < nullInt);
		f(-1 <= nullInt);

		f(nullInt > -1);
		f(nullInt >= -1);
		f(nullInt < -1);
		f(nullInt <= -1);

		var nullIntZero:Null<Int> = 0;

		f(nullIntZero == nullInt);
		f(nullInt == nullIntZero);
		t(nullIntZero != nullInt);
		t(nullInt != nullIntZero);

		f(nullIntZero > nullInt);
		f(nullIntZero >= nullInt);
		f(nullIntZero < nullInt);
		f(nullIntZero <= nullInt);

		f(nullInt > nullIntZero);
		f(nullInt >= nullIntZero);
		f(nullInt < nullIntZero);
		f(nullInt <= nullIntZero);

		var nullIntOne:Null<Int> = 1;

		f(nullIntOne > nullInt);
		f(nullIntOne >= nullInt);
		f(nullIntOne < nullInt);
		f(nullIntOne <= nullInt);

		f(nullInt > nullIntOne);
		f(nullInt >= nullIntOne);
		f(nullInt < nullIntOne);
		f(nullInt <= nullIntOne);

		var nullIntMinusOne:Null<Int> = -1;

		f(nullIntMinusOne > nullInt);
		f(nullIntMinusOne >= nullInt);
		f(nullIntMinusOne < nullInt);
		f(nullIntMinusOne <= nullInt);

		f(nullInt > nullIntMinusOne);
		f(nullInt >= nullIntMinusOne);
		f(nullInt < nullIntMinusOne);
		f(nullInt <= nullIntMinusOne);

		// float
		var nullFloat:Null<Float> = null;

		t(null == nullFloat);
		t(nullFloat == null);
		f(null != nullFloat);
		f(nullFloat != null);

		f(null >  nullFloat);
		f(null >= nullFloat);
		f(null <  nullFloat);
		f(null <= nullFloat);

		f(nullFloat >  null);
		f(nullFloat >= null);
		f(nullFloat <  null);
		f(nullFloat <= null);

		f(0. == nullFloat);
		f(nullFloat == 0.);
		t(0. != nullFloat);
		t(nullFloat != 0.);

		f(0. > nullFloat);
		f(0. >= nullFloat);
		f(0. < nullFloat);
		f(0. <= nullFloat);

		f(nullFloat > 0.);
		f(nullFloat >= 0.);
		f(nullFloat < 0.);
		f(nullFloat <= 0.);

		f(1. > nullFloat);
		f(1. >= nullFloat);
		f(1. < nullFloat);
		f(1. <= nullFloat);

		f(nullFloat > 1.);
		f(nullFloat >= 1.);
		f(nullFloat < 1.);
		f(nullFloat <= 1.);

		f(-1. > nullFloat);
		f(-1. >= nullFloat);
		f(-1. < nullFloat);
		f(-1. <= nullFloat);

		f(nullFloat > -1.);
		f(nullFloat >= -1.);
		f(nullFloat < -1.);
		f(nullFloat <= -1.);

		var nullFloatZero:Null<Float> = 0.;

		f(nullFloatZero == nullFloat);
		f(nullFloat == nullFloatZero);
		t(nullFloatZero != nullFloat);
		t(nullFloat != nullFloatZero);

		f(nullFloatZero > nullFloat);
		f(nullFloatZero >= nullFloat);
		f(nullFloatZero < nullFloat);
		f(nullFloatZero <= nullFloat);

		f(nullFloat > nullFloatZero);
		f(nullFloat >= nullFloatZero);
		f(nullFloat < nullFloatZero);
		f(nullFloat <= nullFloatZero);

		var nullFloatOne:Null<Float> = 1.;
		f(nullFloatOne > nullFloat);
		f(nullFloatOne >= nullFloat);
		f(nullFloatOne < nullFloat);
		f(nullFloatOne <= nullFloat);

		f(nullFloat > nullFloatOne);
		f(nullFloat >= nullFloatOne);
		f(nullFloat < nullFloatOne);
		f(nullFloat <= nullFloatOne);

		var nullFloatMinusOne:Null<Float> = -1.;

		f(nullFloatMinusOne > nullFloat);
		f(nullFloatMinusOne >= nullFloat);
		f(nullFloatMinusOne < nullFloat);
		f(nullFloatMinusOne <= nullFloat);

		f(nullFloat > nullFloatMinusOne);
		f(nullFloat >= nullFloatMinusOne);
		f(nullFloat < nullFloatMinusOne);
		f(nullFloat <= nullFloatMinusOne);
	}

	#end

	@:analyzer(ignore)
	function testDynamicOps() {
		var a:Dynamic = 10;
		// arithmetic
		eq(9., a - 1);
		eq(20., a * 2);
		feq(5., a / 2);
		feq(1., a % 3);

		// bit
		eq(20, a << 1);
		eq(5, a >> 1);
		eq(5, a >>> 1);
		eq(10, a & 15);
		eq(15, a | 15);
		eq(2, a ^ 8);

		// unary
		eq(-10., -a);
		eq(-11, ~a);

		// boolean
		var b:Dynamic = true;
		eq(false, !b);
		eq(false, b && falseValue);
		eq(true, b && trueValue);
		eq(true, b || falseValue);
		eq(true, b || trueValue);

		b = false;
		eq(true, !b);
		eq(false, b && falseValue);
		eq(false, b && trueValue);
		eq(false, b || falseValue);
		eq(true, b || trueValue);

		eq(true, a > 5);
		eq(true, a >= 5);
		eq(false, a < 5);
		eq(false, a <= 5);
		eq(true, a != 5);
		eq(false, a != 10);
	}

	static var trueValue = true;
	static var falseValue = false;

	#end
}
