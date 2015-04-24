package unit.issues;

class Issue2772 extends Test
{
#if java
	public function test()
	{
		var f = false;
		java.Lib.lock(Issue2772, f = true);
		t(f);
		java.Lib.lock(this, f = false);
		this.f(f);
	}
#end
}
