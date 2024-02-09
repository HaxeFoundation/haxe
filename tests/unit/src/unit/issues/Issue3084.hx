package unit.issues;
import unit.Test;

class Issue3084 extends Test
{
#if jvm
	function test()
	{
		for (i in 0...40)
		{
			var m = new sys.thread.Mutex();
			sys.thread.Thread.create(function():Void {
				m.acquire();
				Sys.sleep(0.01);
				m.release();
			});
			sys.thread.Thread.create(function():Void {
				m.acquire();
				Sys.sleep(0.01);
				m.release();
			});

			Sys.sleep(0.01);
			m.acquire();
			Sys.sleep(0.01);
			m.release();
		}
		noAssert();
	}
#end
}
