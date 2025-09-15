package hxcoro.ds.channels.bounded;

import haxe.Exception;
import haxe.coro.IContinuation;
import hxcoro.ds.Out;
import hxcoro.ds.CircularBuffer;
import hxcoro.ds.channels.Channel;
import hxcoro.exceptions.ChannelClosedException;

using hxcoro.util.Convenience;

final class BoundedWriter<T> implements IChannelWriter<T> {
	final closed : Out<Bool>;

	final buffer : CircularBuffer<T>;

	final writeWaiters : PagedDeque<IContinuation<Bool>>;

	final readWaiters : PagedDeque<IContinuation<Bool>>;

	final behaviour : FullBehaviour<T>;

	public function new(buffer, writeWaiters, readWaiters, closed, behaviour) {
		this.buffer        = buffer;
		this.writeWaiters  = writeWaiters;
		this.readWaiters   = readWaiters;
		this.closed        = closed;
		this.behaviour     = behaviour;
	}

	public function tryWrite(v:T):Bool {
		if (closed.get()) {
			return false;
		}

		return if (buffer.tryPush(v)) {
			final cont = new Out();
			while (readWaiters.tryPop(cont)) {
				cont.get().succeedAsync(true);
			}

			true;
		} else {
			false;
		}
	}

	@:coroutine public function write(v:T) {
		if (tryWrite(v)) {
			return;
		}

		switch behaviour {
			case Wait:
				while (waitForWrite()) {
					if (tryWrite(v)) {
						return;
					}
				}
			case DropNewest(f):
				final out = new Out();

				while (tryWrite(v) == false) {
					if (buffer.tryPopHead(out)) {
						f(out.get());
					} else {
						throw new Exception('Failed to drop newest item');
					}
				}

				return;
			case DropOldest(f):
				final out = new Out();

				while (tryWrite(v) == false) {
					if (buffer.tryPopTail(out)) {
						f(out.get());
					} else {
						throw new Exception('Failed to drop oldest item');
					}
				}
				
				return;
			case DropWrite(f):
				f(v);

				return;
		}

		throw new ChannelClosedException();
	}

	@:coroutine public function waitForWrite():Bool {
		if (closed.get()) {
			return false;
		}

		return if (buffer.wasFull()) {
			return suspendCancellable(cont -> {
				final hostPage  = writeWaiters.push(cont);

				cont.onCancellationRequested = _ -> {
					writeWaiters.remove(hostPage, cont);
				}
			});
		} else {
			true;
		}
	}

	public function close() {
		if (closed.get()) {
			return;
		}

		closed.set(true);

		while (writeWaiters.isEmpty() == false) {
			switch writeWaiters.pop() {
				case null:
					continue;
				case cont:
					cont.succeedAsync(false);
			}
		};

		while (readWaiters.isEmpty() == false) {
			switch (readWaiters.pop()) {
				case null:
					continue;
				case cont:
					cont.succeedAsync(false);
			}
		};
	}
}
