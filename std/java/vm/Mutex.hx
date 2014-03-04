package java.vm;

@:native('haxe.java.vm.Mutex') class Mutex
{
	@:private var owner:java.lang.Thread;
	@:private var lockCount:Int = 0;

	/**
		Creates a mutex, which can be used to acquire a temporary lock to access some resource.
		The main difference with a lock is that a mutex must always be released by the owner thread
	**/
	public function new()
	{

	}

	/**
		Try to acquire the mutex, returns true if acquire or false if it's already locked by another thread.
	**/
	public function tryAcquire():Bool
	{
		var ret = false, cur = java.lang.Thread.currentThread();
		untyped __lock__(this, {
			var expr = null;
			if (owner == null)
			{
				ret = true;
				if(lockCount != 0) throw "assert";
				lockCount = 1;
				owner = cur;
			} else if (owner == cur) {
				ret = true;
				owner = cur;
				lockCount++;
			}
		});
		return ret;
	}

	/**
		The current thread acquire the mutex or wait if not available.
		The same thread can acquire several times the same mutex, but must release it as many times it has been acquired.
	**/
	public function acquire():Void
	{
		var cur = java.lang.Thread.currentThread();
		untyped __lock__(this, {
			var expr = null;
			if (owner == null)
			{
				owner = cur;
				if (lockCount != 0) throw "assert";
				lockCount = 1;
			} else if (owner == cur) {
				lockCount++;
			} else {
				try { untyped this.wait(); } catch(e:Dynamic) { throw e; }
				lockCount = 1;
				owner = cur;
			}
		});
	}

	/**
		Release a mutex that has been acquired by the current thread. If the current thread does not own the mutex, an exception will be thrown
	**/
	public function release():Void
	{
		var cur = java.lang.Thread.currentThread();
		untyped __lock__(this, {
			var expr = null;
			if (owner != cur) {
				throw "This mutex isn't owned by the current thread!";
			}
			if (--lockCount == 0)
			{
				this.owner = null;
				untyped this.notify();
			}
		});
	}
}
