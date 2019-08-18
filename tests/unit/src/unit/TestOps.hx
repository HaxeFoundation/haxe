package unit;

class TestOps extends Test {

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

}
