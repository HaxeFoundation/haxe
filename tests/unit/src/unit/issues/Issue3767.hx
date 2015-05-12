package unit.issues;
import unit.Test;
#if java
import java.vm.*;
#elseif neko
import neko.vm.*;
#elseif cpp
import cpp.vm.*;
#end

class Issue3767 extends Test
{
#if (java || (neko && !interp && !macro) || (cpp && !emscripten))
	function testBasicLock()
	{
		var lock = new Lock();
		//it starts locked
		f(lock.wait(0.001));
		lock.release();
		t(lock.wait(.001));
		f(lock.wait(.001));
		f(lock.wait(.001));

		lock.release();
		t(lock.wait());
		lock.release();
		lock.release();
		lock.release();
		t(lock.wait());
		t(lock.wait(.001));
		t(lock.wait());
		f(lock.wait(.001));

		var cur = Sys.time();
		Thread.create(function()
		{
			Sys.sleep(.01);
			lock.release();
		});
		t(lock.wait(2.0));
		t( (Sys.time() - cur) < 2 );
		Thread.create(function()
		{
			Sys.sleep(.01);
			lock.release();
		});
		t(lock.wait());
	}
#end
}
