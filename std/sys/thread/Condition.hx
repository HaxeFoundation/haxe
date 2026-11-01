package sys.thread;

#if (!target.threaded)
#error "This class is not available on this target"
#end

/**
	Creates a new condition variable.
	Conditions variables can be used to block one or more threads at the same time,
	until another thread modifies a shared variable (the condition)
	and signals the condition variable.

	# Example
	```haxe
	import sys.thread.Thread;
	import sys.thread.Condition;

	function main() {
		var started = false;
		final cond = new Condition();
		Thread.create(() -> {
			cond.acquire();
			started = true;
			cond.signal();
			cond.release();
		});
		// Wait for the thread to start up
		cond.acquire();
		while(!started) {
			cond.wait();
		}
		cond.release();
	}
	```
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
		Blocks the current thread until this condition variable receives a notification or a spurious wakeup occurs.

		The internal mutex is atomically released and the current thread blocked until the condition variable is signaled by a call to
		`signal` or `broadcast`, or a spurious wakeup occurs.
		When this function returns, the internal mutex will have been re-acquired.

		This function is susceptible to spurious wakeups.
		Condition variables should be associated with a boolean predicate,
		and this function should be called in a loop that checks the predicate to protect against spurious wakeups.

		The internal mutex must be locked before this function is called.
	**/
	function wait():Void;

	/**
		Unblocks one of the threads that are blocked on the
		condition variable at the time of the call. If no threads are blocked
		on the condition variable at the time of the call, the function does nothing.

		The internal mutex must be locked before this function is called.
	**/
	function signal():Void;

	/**
		Unblocks all of the threads that are blocked on the
		condition variable at the time of the call. If no threads are blocked
		on the condition variable at the time of the call, the function does
		nothing.

		The internal mutex must be locked before this function is called.
	**/
	function broadcast():Void;
}
