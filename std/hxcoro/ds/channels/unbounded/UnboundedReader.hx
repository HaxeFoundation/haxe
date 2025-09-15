package hxcoro.ds.channels.unbounded;

import haxe.Exception;
import haxe.coro.IContinuation;
import haxe.coro.context.Context;
import hxcoro.ds.Out;
import hxcoro.exceptions.ChannelClosedException;

private final class WaitContinuation<T> implements IContinuation<Bool> {
	final cont : IContinuation<Bool>;

	final buffer : PagedDeque<T>;

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

			cont.succeedAsync(buffer.isEmpty());
		} else {
			cont.succeedAsync(true);
		}
	}
}

final class UnboundedReader<T> implements IChannelReader<T> {
	final buffer : PagedDeque<T>;

	final readWaiters : PagedDeque<IContinuation<Bool>>;

	final closed : Out<Bool>;

	public function new(buffer, readWaiters, closed) {
		this.buffer      = buffer;
		this.readWaiters = readWaiters;
		this.closed      = closed;
	}

	public function tryRead(out:Out<T>):Bool {
		return buffer.tryPop(out);
	}

	public function tryPeek(out:Out<T>):Bool {
		return buffer.tryPeek(out);
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
		if (buffer.isEmpty() == false) {
			return true;
		}

		if (closed.get()) {
			return false;
		}

		return suspendCancellable(cont -> {
			final obj       = new WaitContinuation(cont, buffer, closed);
			final hostPage  = readWaiters.push(obj);

			cont.onCancellationRequested = _ -> {
				readWaiters.remove(hostPage, obj);
			}
		});
	}
}