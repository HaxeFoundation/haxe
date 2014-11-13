package unit.issues;

class Issue2725 extends unit.Test
{
	public function test()
	{
		var x:Dynamic = null;
		var b:Bool = x;
		t(!b);
	}
}
