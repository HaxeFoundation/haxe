package java.vm;
import java.Lib;

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
		untyped __lock__(this,
		{
			if (--releasedCount >= 0)
				return true;
			if (timeout == null)
			{
				try
				{
					untyped __java__("this.wait()");
				}
				catch(e:Dynamic) {
					throw e;
				}
			} else {
				var t:Dynamic = this;
				try
				{
					var t:haxe.Int64 = cast timeout * 1000;
					untyped __java__("this.wait(t)");
				}
				catch(e:Dynamic) {
					throw e;
				}
			}
			ret = this.releasedCount >= 0;
			if (!ret && ++this.releasedCount == 0) //even if timeout failed, we should release the lock; Other locks may be released then
			{
				untyped this.notify();
			}
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
