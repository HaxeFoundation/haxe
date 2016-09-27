package unit.issues;

class Issue4407 extends Test
{
	public function test()
	{
		// please don't optimize this too much
		var ex:Dynamic = Example;
		t( Type.enumEq( Type.createEnum(ex, "NullParam", untyped [null, "123"]), Example.NullParam(null,"123")) );
	}
}

private enum Example
{
	NullParam(first:Null<Int>, second:String);
}
