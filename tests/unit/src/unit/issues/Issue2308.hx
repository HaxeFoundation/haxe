package unit.issues;

class Issue2308 extends Test
{
	public function test()
	{
		var fn = getValue;
		var val = Reflect.callMethod(null, fn, [ETest2.A2]);
		eq(val,0);
	}

	private static function getValue(e:ETest2):Int
	{
		return Type.enumIndex(e);
	}
}

@:nativeGen private enum ETest2
{
	A2;
	B2;
}
