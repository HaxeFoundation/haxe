package unit.issues;

class Issue4051 extends Test
{
	public function test()
	{
#if !flash
		eq(Std.parseFloat("1154874.2868745863"), 1154874.2868745863);
#end
	}
}
