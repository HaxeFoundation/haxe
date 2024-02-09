package unit.issues;

class Issue3578 extends Test
{
	function test()
	{
		var i = 0;
		var t = new TestG(function() i++);
		eq(i,1);
		t.func();
		eq(i,2);
	}
}

private class TestG
{
	public var func:()->Void;
	public function new(callback:()->Void)
	{
		function x()
		{
			callback();
		}
		x();
		this.func = x;
	}
}
