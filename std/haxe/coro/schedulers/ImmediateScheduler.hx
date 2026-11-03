package haxe.coro.schedulers;

import haxe.coro.dispatchers.Dispatcher;
import haxe.Int64;
import haxe.coro.IContinuation;
import haxe.coro.schedulers.IScheduler;
import haxe.coro.schedulers.ISchedulerHandle;
import haxe.coro.dispatchers.IDispatchObject;

private class ContinuationDispatchObject implements IDispatchObject {
	public final cont:IContinuation<Any>;

	public function new(cont) {
		this.cont = cont;
	}

	public function onDispatch() {
		cont.resume(null, null);
	}
}

/**
	A scheduler that dispatches continuations immediately.
**/
class ImmediateScheduler implements IScheduler implements ISchedulerHandle {
	/**
		Creates a new `ImmediateScheduler` instance.
	**/
	public function new() {}

	/**
		Dispatches `cont` immediately, ignoring `ms`.
	**/
	public function schedule(ms:Int64, cont:IContinuation<Any>) {
		cont.context.get(Dispatcher).dispatch(new ContinuationDispatchObject(cont));
		return this;
	}

	/**
		Returns 0.
	**/
	public function now() {
		return 0i64;
	}

	/**
		Does nothing.
	**/
	public function close() {}
}
