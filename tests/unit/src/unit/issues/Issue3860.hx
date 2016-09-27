package unit.issues;

class Issue3860 extends Test
{
	public function test()
	{
		var a = ETest.A;
		t((a is ETest));
		f((a is Int));

		var dyn:Dynamic = a;
		t((dyn is ETest));
		f((dyn is Int));
	}
}

private enum ETest
{
	A;
	B;
}
