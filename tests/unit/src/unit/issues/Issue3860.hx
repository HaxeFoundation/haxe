package unit.issues;

class Issue3860 extends Test
{
	public function test()
	{
		var a = ETest.A;
		t(Std.is(a,ETest));
		f(Std.is(a,Int));

		var dyn:Dynamic = a;
		t(Std.is(dyn,ETest));
		f(Std.is(dyn,Int));
	}
}

private enum ETest
{
	A;
	B;
}
