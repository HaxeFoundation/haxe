package unit;

class TestUnspecified extends Test
{
	public function testModuloZero()
	{
		#if neko
			exc(function() 1 % 0);
		#elseif cpp
			// Crashes
		#elseif (php || flash9)
			eq(1 % 0, 0);
		#else
			t(Math.isNaN(1 % 0));
		#end
	}

	public function testMonoAdd()
	{
		var x : Array<Dynamic> = ["4", 1];
		#if (php || flash9)
			t(Std.is(x[0] + x[0], Int));
			t(Std.is(x[0] + x[1], Int));
		#else
			t(Std.is(x[0] + x[0], String));
			t(Std.is(x[0] + x[1], String));
		#end
	}
}