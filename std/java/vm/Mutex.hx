package java.vm;
import java.util.concurrent.Semaphore;

@:native('haxe.java.vm.Mutex') class Mutex
{
	@:private var lock:Semaphore;

	/**
		Creates a mutex, which can be used to acquire a temporary lock to access some resource.
		The main difference with a lock is that a mutex must always be released by the owner thread
	**/
	public function new()
	{
		this.lock = new Semaphore(1);
	}

	/**
		Try to acquire the mutex, returns true if acquire or false if it's already locked by another thread.
	**/
	public function tryAcquire():Bool
	{
		return this.lock.tryAcquire();
	}

	/**
		The current thread acquire the mutex or wait if not available.
		The same thread can acquire several times the same mutex, but must release it as many times it has been acquired.
	**/
	public function acquire():Void
	{
		this.lock.acquire();
	}

	/**
		Release a mutex that has been acquired by the current thread. If the current thread does not own the mutex, an exception will be thrown
	**/
	public function release():Void
	{
		this.lock.release();
	}
}
