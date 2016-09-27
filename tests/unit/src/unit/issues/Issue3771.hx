package unit.issues;

class Issue3771 extends Test
{
	public function test()
	{
#if cs
		var arr = [test];
		eq(arr.indexOf(test),0);
#end
	}
}
