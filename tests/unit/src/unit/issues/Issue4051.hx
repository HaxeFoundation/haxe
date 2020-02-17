package unit.issues;

class Issue4051 extends Test
{
	#if !flash
	public function test()
	{
		eq(Std.parseFloat("1154874.2868745863"), 1154874.2868745863);
	}
	#end
}
