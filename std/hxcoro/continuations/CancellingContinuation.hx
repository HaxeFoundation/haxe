package hxcoro.continuations;

import haxe.coro.SuspensionResult;
import haxe.coro.schedulers.IScheduleObject;
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

class CancellingContinuation<T> extends SuspensionResult<T> implements ICancellableContinuation<T> implements ICancellationCallback implements IScheduleObject {
	final resumeState : AtomicInt;

	final cont : IContinuation<T>;

	final handle : ICancellationHandle;

	public var context (get, never) : Context;

	function get_context() {
		return cont.context;
	}

	public var onCancellationRequested (default, set) : CancellationException->Void;

	function set_onCancellationRequested(f : CancellationException->Void) {
		return switch (cont.context.get(CancellationToken).cancellationException) {
			case null:
				if (null != onCancellationRequested) {
					throw new Exception("Callback already registered");
				}

				onCancellationRequested = f;
			case exc:
				f(exc);

				f;
		}
	}

	public function new(cont) {
		this.resumeState  = new AtomicInt(Active);
		this.cont   = cont;
		this.handle = this.cont.context.get(CancellationToken).onCancellationRequested(this);
		this.state  = Pending;
	}

	public function resume(result:T, error:Exception) {
		this.result = result;
		this.error = error;
		if (resumeState.compareExchange(Active, Resumed) == Active) {
			handle.close();
			context.get(Scheduler).scheduleObject(this);
		} else {
			cont.failAsync(error.orCancellationException());
		}

	}

	public function onCancellation(cause:CancellationException) {
		handle?.close();

		if (resumeState.compareExchange(Active, Cancelled) == Active) {
			if (null != onCancellationRequested) {
				onCancellationRequested(cause);
			}

			cont.failAsync(cause);
		}
	}

	public function onSchedule() {
		cont.resume(result, error);
	}
}