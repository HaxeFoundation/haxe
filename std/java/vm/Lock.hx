package java.vm;
import java.Lib;
import java.lang.System;
using haxe.Int64;

@:native('haxe.java.vm.Lock') class Lock
{
	@:private @:volatile var releasedCount = 0;

	/**
		Creates a new lock, which is initially locked
	**/
	public function new()
	{
	}

	/**
		Waits for a lock to be released and acquire it.
		If `timeout` (in seconds) is not null and expires then the returned value is false
	**/
	public function wait(?timeout : Float) : Bool
	{
		var ret = false;
		java.Lib.lock(this,
		{
			if (--releasedCount < 0)
			{
				if (timeout == null)
				{
					// since .notify() is asynchronous, this `while` is needed
					// because there is a very remote possibility of release() awaking a thread,
					// but before it releases, another thread calls wait - and since the release count
					// is still positive, it will get the lock.
					while( releasedCount < 0 )
					{
						try
						{
							untyped __java__("this.wait()");
						}
						catch(e:java.lang.InterruptedException)
						{
						}
					}
				} else {
					var timeout:haxe.Int64 = cast timeout * 1000;
					var cur = System.currentTimeMillis(),
					    max = cur.add(timeout);
					// see above comment about this while loop
					while ( releasedCount < 0 && cur.compare(max) < 0 )
					{
						try
						{
							var t = max.sub(cur);
							untyped __java__("this.wait({0})",t);
							cur = System.currentTimeMillis();
						}
						catch(e:java.lang.InterruptedException)
						{
						}
					}
				}
			}
			ret = this.releasedCount >= 0;
			if (!ret)
				this.releasedCount++; //timed out
		});
		return ret;
	}

	/**
		Release a lock. The thread does not need to own the lock to be able to release it.
		If a lock is released several times, it can be acquired as many times
	**/
	public function release()
	{
		untyped __lock__(this,
		{
			if (++releasedCount >= 0)
			{
				untyped this.notify();
			}
		});
	}
}
