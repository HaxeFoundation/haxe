package unit.issues;

class Issue3771 extends Test
{
	#if cs
	public function test()
	{
		var arr = [test];
		eq(arr.indexOf(test),0);
	}
	#end
}
