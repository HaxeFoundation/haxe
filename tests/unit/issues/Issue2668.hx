package unit.issues;

class Issue2668 extends unit.Test
{
	public function test()
	{
		var a:Dynamic = 1.5;
		var b:Dynamic = "s";
		f(a == b);
	}
}
