package hxcoro.ds;

import haxe.coro.ICancellableContinuation;
import haxe.Exception;
import haxe.exceptions.CancellationException;
import haxe.coro.cancellation.CancellationToken;
import haxe.coro.context.Context;
import haxe.coro.IContinuation;
import hxcoro.Coro.suspendCancellable;
import hxcoro.ds.PagedDeque;

private class SuspendedWrite<T> implements IContinuation<T> {
	final continuation : IContinuation<T>;

	public final value : T;

	public var context (get, never) : Context;

	var hostPage:Page<Any>;
	var hostIndex:Int;

	inline function get_context() {
		return continuation.context;
	}

	public function new(continuation:ICancellableContinuation<T>, value, suspendedWrites:PagedDeque<Any>) {
		this.continuation = continuation;
		this.value        = value;
		// writeMutex.acquire();
		hostPage = suspendedWrites.push(this);
		hostIndex = suspendedWrites.lastIndex - 1;
		// writeMutex.release();
		continuation.onCancellationRequested = onCancellation;
	}

	public function resume(v:T, error:Exception) {
		if (context.get(CancellationToken).isCancellationRequested) {
			continuation.failAsync(new CancellationException());
		} else {
			continuation.resume(v, error);
		}
	}

	function onCancellation() {
		// writeMutex.acquire();
		if (hostPage.data[hostIndex] == this) {
			hostPage.data[hostIndex] = null;
		}
		// writeMutex.release();
		this.callSync();
	}
}

class SuspendedRead<T> implements IContinuation<T> {
	final continuation : IContinuation<T>;

	public var context (get, never) : Context;

	var hostPage:Page<Any>;
	var hostIndex:Int;

	inline function get_context() {
		return continuation.context;
	}

	public function new(continuation:ICancellableContinuation<T>, suspendedReads:PagedDeque<Any>) {
		this.continuation = continuation;

		// readMutex.acquire();
		hostPage = suspendedReads.push(this);
		hostIndex = suspendedReads.lastIndex - 1;
		// readMutex.release();
		continuation.onCancellationRequested = onCancellation;
	}

	public function resume(v:T, error:Exception) {
		if (context.get(CancellationToken).isCancellationRequested) {
			continuation.failAsync(new CancellationException());
		} else {
			continuation.resume(v, error);
		}
	}

	function onCancellation() {
		// readMutex.acquire();
		if (hostPage.data[hostIndex] == this) {
			hostPage.data[hostIndex] = null;
		}
		// readMutex.release();
		this.callSync();
	}
}

class Channel<T> {
	final bufferSize : Int;
	final writeQueue : Array<T>;
	final suspendedWrites : PagedDeque<SuspendedWrite<T>>;
	final suspendedReads : PagedDeque<SuspendedRead<T>>;

	/**
		Creates a new empty Channel.
	**/
	public function new(bufferSize = 3) {
		this.bufferSize = bufferSize;

		writeQueue      = [];
		suspendedWrites = new PagedDeque();
		suspendedReads  = new PagedDeque();
	}

	/**
		Writes `v` to this channel. If the operation cannot be completed immediately, execution is
		suspended. It can be resumed by a later call to `read`.
	**/
	@:coroutine public function write(v:T) {
		while (true) {
			if (suspendedReads.isEmpty()) {
				if (writeQueue.length < bufferSize) {
					writeQueue.push(v);
				} else {
					suspendCancellable(cont -> {
						new SuspendedWrite(cont, v, suspendedWrites);
					});
				}
				break;
			} else {
				final suspendedRead = suspendedReads.pop();
				if (suspendedRead == null) {
					continue;
				} else {
					suspendedRead.succeedAsync(v);
					break;
				}
			}
		}
	}

	/**
		Reads an element from this channel. If the operation cannot be completed immediately,
		execution is suspended. It can be resumed by a later call to `write`.
	**/
	@:coroutine public function read():T {
		while ((bufferSize == 0 || writeQueue.length < bufferSize) && !suspendedWrites.isEmpty()) {
			final resuming = suspendedWrites.pop();
			if (resuming == null) {
				continue;
			}
			resuming.callSync();
			if (writeQueue.length == 0) {
				return resuming.value;
			} else {
				writeQueue.push(resuming.value);
			}
		}
		switch writeQueue.shift() {
			case null:
				return suspendCancellable(cont -> {
					new SuspendedRead(cont, suspendedReads);
				});
			case v:
				return v;
		}
	}
}
