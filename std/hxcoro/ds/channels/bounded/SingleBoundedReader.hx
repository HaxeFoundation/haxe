package hxcoro.ds.channels.bounded;

import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.context.Context;
import hxcoro.ds.Out;
import hxcoro.exceptions.ChannelClosedException;
import hxcoro.ds.ConcurrentCircularBuffer;
import hxcoro.concurrent.AtomicObject;

using hxcoro.util.Convenience;

private final class WaitContinuation<T> implements IContinuation<Bool> {
	final cont : IContinuation<Bool>;

	final buffer : ConcurrentCircularBuffer<T>;

	final closed : Out<Bool>;

	public var context (get, never) : Context;

	function get_context() {
		return cont.context;
	}

	public function new(cont, buffer, closed) {
		this.cont   = cont;
		this.buffer = buffer;
		this.closed = closed;
	}

	public function resume(result:Bool, error:Exception) {
		if (false == result) {
			closed.set(false);

			cont.succeedAsync(buffer.wasEmpty());
		} else {
			cont.succeedAsync(true);
		}
	}
}

final class SingleBoundedReader<T> implements IChannelReader<T> {
	final buffer : ConcurrentCircularBuffer<T>;

	final writeWaiter : AtomicObject<IContinuation<Bool>>;

	final readWaiter : AtomicObject<IContinuation<Bool>>;

	final closed : Out<Bool>;

	final readOut : Out<T>;

	public function new(buffer, writeWaiter, readWaiter, closed) {
		this.buffer      = buffer;
		this.writeWaiter = writeWaiter;
		this.readWaiter  = readWaiter;
		this.closed      = closed;
		this.readOut     = new Out();
	}

	public function tryRead(out:Out<T>):Bool {
		return if (buffer.tryPop(readOut)) {

			writeWaiter.exchange(null)?.succeedAsync(true);
			
			true;
		} else {
			false;
		}
	}

	public function tryPeek(out:Out<T>):Bool {
		throw new haxe.exceptions.NotImplementedException();
	}

	@:coroutine public function read():T {
		final out = new Out();

		while (true)
		{
			if (waitForRead() == false) {
				throw new ChannelClosedException();
			}

			if (tryRead(out)) {
				return out.get();
			}
		}
	}

	@:coroutine public function waitForRead():Bool {
		if (buffer.wasEmpty() == false) {
			return true;
		}

		if (closed.get()) {
			return false;
		}

		return suspendCancellable(cont -> {
			final obj       = new WaitContinuation(cont, buffer, closed);
			final hostPage  = readWaiter.store(obj);

			cont.onCancellationRequested = _ -> {
				readWaiter.store(null);
			}
		});
	}
}