package sys.thread;

#if (!target.threaded)
#error "This class is not available on this target"
#end

/**
	Creates a new condition variable.
	Conditions variables can be used to block one or more threads at the same time,
	until another thread modifies a shared variable (the condition)
	and signals the condition variable.
**/
@:coreApi extern class Condition {
	/**
		Create a new condition variable.
		A thread that waits on a newly created condition variable will block.
	**/
	function new():Void;

	/**
		Acquires the internal mutex.
	**/
	function acquire():Void;

	/**
		Tries to acquire the internal mutex.
		@see `Mutex.tryAcquire`
	**/
	function tryAcquire():Bool;

	/***
		Releases the internal mutex.
	**/
	function release():Void;

	/**
		Atomically releases the mutex and blocks until the condition variable pointed is signaled by a call to
		`signal` or to `broadcast`. When the calling thread becomes unblocked it
		acquires the internal mutex.
		The internal mutex should be locked before this function is called.
	**/
	function wait():Void;

	/**
		Unblocks one of the threads that are blocked on the
		condition variable at the time of the call. If no threads are blocked
		on the condition variable at the time of the call, the function does nothing.
	**/
	function signal():Void;

	/**
		Unblocks all of the threads that are blocked on the
		condition variable at the time of the call. If no threads are blocked
		on the condition variable at the time of the call, the function does
		nothing.
	**/
	function broadcast():Void;
}
