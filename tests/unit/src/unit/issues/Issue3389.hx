package unit.issues;

class Issue3389 extends Test
{
	public function test()
	{
		eq(Type.allEnums(ETest).join(','),'A,B');
		eq(Type.allEnums(ETest2).join(','),'A2,B2');
	}
}

private enum ETest
{
	A;
	B;
}

@:nativeGen private enum ETest2
{
	A2;
	B2;
}
