package hxcoro.continuations;

import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.context.Context;
import haxe.coro.schedulers.ISchedulerHandle;

class TimeoutContinuation<T> implements IContinuation<T> {
	final cont : IContinuation<T>;
	final handle : ISchedulerHandle;

	public var context (get, never) : Context;

	inline function get_context() {
		return cont.context;
	}

	public function new(cont, handle) {
		this.cont   = cont;
		this.handle = handle;
	}

	public function resume(value:T, error:Exception) {
		handle.close();

		cont.resumeAsync(value, error);
	}
}