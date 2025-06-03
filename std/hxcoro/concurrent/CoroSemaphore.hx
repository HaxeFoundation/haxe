package hxcoro.concurrent;

import haxe.coro.Mutex;
import hxcoro.Coro.*;
import hxcoro.task.CoroTask;
import hxcoro.ds.PagedDeque;
import haxe.coro.IContinuation;
import haxe.coro.cancellation.ICancellationHandle;
import haxe.exceptions.CancellationException;
import haxe.coro.cancellation.CancellationToken;

@:structInit
private class PendingAcquire<T> {
	public final cont:IContinuation<T>;
	public final cancelHandle:ICancellationHandle;
}

class CoroSemaphore {
	final maxFree:Int;
	final dequeMutex:Mutex;
	var deque:Null<PagedDeque<PendingAcquire<Any>>>;
	var free:AtomicInt;

	public function new(free:Int) {
		maxFree = free;
		dequeMutex = new Mutex();
		this.free = new AtomicInt(free);
	}

	@:coroutine public function acquire() {
		if (free.sub(1) > 0) {
			return;
		}
		suspend(cont -> {
			final f = () -> cont.resume(null, new CancellationException());
			final task = cont.context.get(CoroTask.key);
			dequeMutex.acquire();
			if (deque == null) {
				deque = new PagedDeque();
			}
			deque.push({cont: cont, cancelHandle: task.onCancellationRequested(f)});
			task.putOnHold(); // TODO: condition this on some heuristic?
			dequeMutex.release();
		});
	}

	public function tryAcquire() {
		while (true) {
			var free = free.load();
			if (free <= 0) {
				return false;
			}
			if (this.free.compareExchange(free, free - 1) == free) {
				return true;
			}
		}
	}

	public function release() {
		free.add(1);
		dequeMutex.acquire();
		if (deque == null) {
			dequeMutex.release();
			return;
		}
		while (true) {
			if (deque.isEmpty()) {
				// nobody else wants it right now, return
				dequeMutex.release();
				return;
			}
			// a continuation waits for this mutex, wake it up now
			final acq = deque.pop();
			final cont = acq.cont;
			final ct = cont.context.get(CancellationToken.key);
			if (ct.isCancellationRequested) {
				// ignore, back to the loop
			} else {
				// continue normally
				dequeMutex.release();
				acq.cancelHandle.close();
				cont.resume(null, null);
				return;
			}
		}
	}
}
