package hxcoro.ds;

import haxe.coro.IContinuation;
import hxcoro.Coro.suspend;

class Channel<T> {
	final maxQueueSize = 3;
	final writeQueue = new Array<T>();
	final suspendedWriteConts = new PagedDeque<IContinuation<Any>>();
	final suspendedWriteValues = new PagedDeque<T>();
	final suspendedReads = new PagedDeque<IContinuation<T>>();

	/**
		Creates a new empty Channel.
	**/
	public function new() {}

	/**
		Writes `v` to this channel. If the operation cannot be completed immediately, execution is
		suspended. It can be resumed by a later call to `read`.
	**/
	@:coroutine public function write(v:T) {
		if (suspendedReads.isEmpty()) {
			if (writeQueue.length < maxQueueSize) {
				writeQueue.push(v);
			} else {
				suspend(cont -> {
					suspendedWriteConts.push(cont);
					suspendedWriteValues.push(v);
				});
			}
		} else {
			suspendedReads.pop().resume(v, null);
		}
	}

	/**
		Reads an element from this channel. If the operation cannot be completed immediately,
		execution is suspended. It can be resumed by a later call to `write`.
	**/
	@:coroutine public function read():T {
		while (writeQueue.length < maxQueueSize && !suspendedWriteConts.isEmpty()) {
			final value = suspendedWriteValues.pop();
			suspendedWriteConts.pop().resume(null, null);
			if (writeQueue.length == 0) {
				return value;
			} else {
				writeQueue.push(value);
			}
		}
		switch writeQueue.shift() {
			case null:
				return suspend(cont -> {
					suspendedReads.push(cont);
				});
			case v:
				return v;
		}
	}
}
