package unit.issues;
import unit.Test;

class Issue3084 extends Test
{
#if java
	function test()
	{
		for (i in 0...40)
		{
			var m = new java.vm.Mutex();
			java.vm.Thread.create(function():Void {
				m.acquire();
				Sys.sleep(0.01);
				m.release();
			});
			java.vm.Thread.create(function():Void {
				m.acquire();
				Sys.sleep(0.01);
				m.release();
			});

			Sys.sleep(0.01);
			m.acquire();
			Sys.sleep(0.01);
			m.release();
		}
	}
#end
}
