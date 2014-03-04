package unit.issues;

class Issue2663 extends unit.Test
{
	public function test()
	{
		var a:Null<Int> = 0;
		var b:Null<Float> = a;
		eq(0,a);
		eq(.0,b);
		eq(b,a);

		a = null;
		b = a;
		eq(null,a);
		eq(null,b);
		eq(b,a);

		a = 1;
		var x = 10;
		b = { ++x; a; };
		eq(11,x);
		eq(1,a);
		eq(1.,b);
		eq(b,a);

		function getValue():Null<Int>
		{
			x++;
			return 2;
		}
		b = getValue();
		eq(12,x);
		eq(1,a);
		a = getValue();
		eq(2,a);
		eq(2.,b);
		eq(b,a);
		eq(b,getValue());
		eq(14,x);
		a = getValue();
		eq(a,getValue());
		eq(16,x);
	}
}
