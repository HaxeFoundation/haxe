package hxcoro.ds.channels.unbounded;

import haxe.coro.IContinuation;
import hxcoro.Coro;
import hxcoro.ds.Out;
import hxcoro.ds.PagedDeque;
import hxcoro.exceptions.ChannelClosedException;

using hxcoro.util.Convenience;

final class UnboundedWriter<T> implements IChannelWriter<T> {
	final closed : Out<Bool>;

	final buffer : PagedDeque<T>;

	final readWaiters : PagedDeque<IContinuation<Bool>>;

	public function new(buffer, readWaiters, closed) {
		this.buffer      = buffer;
		this.readWaiters = readWaiters;
		this.closed      = closed;
	}

	public function tryWrite(v:T):Bool {
		if (closed.get()) {
			return false;
		}

		final _ = buffer.push(v);

		final cont = new Out();
		while (readWaiters.tryPop(cont)) {
			cont.get().succeedAsync(true);
		}

		return true;
	}

	@:coroutine public function waitForWrite():Bool {
		checkCancellation();

		if (closed.get()) {
			return false;
		}

		return true;
	}

	@:coroutine public function write(v:T) {
		while (waitForWrite()) {
			if (tryWrite(v)) {
				return;
			}
		}

		throw new ChannelClosedException();
	}

	public function close() {
		if (closed.get()) {
			return;
		}

		closed.set(true);

		final cont = new Out();
		while (readWaiters.tryPop(cont)) {
			cont.get().succeedAsync(false);
		}
	}

	@:coroutine function checkCancellation() {
		return Coro.suspendCancellable(cont -> {
			cont.succeedAsync(null);
		});
	}
}