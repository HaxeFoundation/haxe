package haxe.coro;

#if (target.threaded)
typedef Mutex = sys.thread.Mutex;
#else
typedef Mutex = StubMutex;

/**
    This is a stub version.
	Creates a mutex, which can be used to acquire a temporary lock
	to access some resource. The main difference with a lock is
	that a mutex must always be released by the owner thread.
**/
class StubMutex {
    /**
		Creates a stub mutex on non threaded target.
	**/
	public function new():Void {}

	/**
        This is a stub version.
		The current thread acquire the mutex or wait if not available.
		The same thread can acquire several times the same mutex but
		must release it as many times it has been acquired.
	**/
	public function acquire():Void {}

	/**
        This is a stub version.
		Try to acquire the mutex, returns true if acquire or false
		if it's already locked by another thread.
	**/
	public function tryAcquire():Bool {
		return true;
	}

	/**
        This is a stub version.
		Release a mutex that has been acquired by the current thread.
		The behavior is undefined if the current thread does not own
		the mutex.
	**/
	public function release():Void {}
}
#end
