package hxcoro.continuations;

import hxcoro.concurrent.AtomicInt;
import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.coro.IContinuation;
import haxe.coro.ICancellableContinuation;
import haxe.coro.context.Context;
import haxe.coro.schedulers.Scheduler;
import haxe.coro.cancellation.ICancellationHandle;
import haxe.coro.cancellation.CancellationToken;
import haxe.coro.cancellation.ICancellationCallback;

private enum abstract State(Int) to Int {
	var Active;
	var Resumed;
	var Cancelled;
}

class CancellingContinuation<T> implements ICancellableContinuation<T> implements ICancellationCallback {
	final state : AtomicInt;

	final cont : IContinuation<T>;

	final handle : ICancellationHandle;

	public var context (get, never) : Context;

	function get_context() {
		return cont.context;
	}

	public var onCancellationRequested (default, set) : ()->Void;

	function set_onCancellationRequested(f : ()->Void) {
		return if (cont.context.get(CancellationToken.key).isCancellationRequested) {
			f();

			f;
		} else {
			if (null != onCancellationRequested) {
				throw new Exception("Callback already registered");
			}

			onCancellationRequested = f;
		}

	}

	public function new(cont) {
		this.state  = new AtomicInt(Active);
		this.cont   = cont;
		this.handle = this.cont.context.get(CancellationToken.key).onCancellationRequested(this);
	}

	public function resume(result:T, error:Exception) {
		context.get(Scheduler.key).schedule(0, () -> {
			if (state.compareExchange(Active, Resumed) == Active) {
				handle.close();
				cont.resume(result, error);
			} else {
				cont.failAsync(new CancellationException());
			}
		});
	}

	public function onCancellation() {
		handle?.close();

		if (state.compareExchange(Active, Cancelled) == Active) {
			if (null != onCancellationRequested) {
				onCancellationRequested();
			}

			resume(null, null);
		}
	}
}