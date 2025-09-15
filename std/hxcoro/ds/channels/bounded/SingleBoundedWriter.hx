package hxcoro.ds.channels.bounded;

import haxe.Exception;
import hxcoro.ds.ConcurrentCircularBuffer;
import hxcoro.concurrent.AtomicObject;
import haxe.coro.IContinuation;
import hxcoro.ds.Out;
import hxcoro.ds.channels.Channel;
import hxcoro.exceptions.ChannelClosedException;

using hxcoro.util.Convenience;

final class SingleBoundedWriter<T> implements IChannelWriter<T> {
	final closed : Out<Bool>;

	final buffer : ConcurrentCircularBuffer<T>;

	final writeWaiter : AtomicObject<IContinuation<Bool>>;

	final readWaiter : AtomicObject<IContinuation<Bool>>;

	final behaviour : FullBehaviour<T>;

	final writeOut : Out<T>;

	public function new(buffer, writeWaiter, readWaiter, closed, behaviour) {
		this.buffer      = buffer;
		this.writeWaiter = writeWaiter;
		this.readWaiter  = readWaiter;
		this.closed      = closed;
		this.behaviour   = behaviour;
		this.writeOut    = new Out();
	}

	public function tryWrite(v:T):Bool {
		if (closed.get()) {
			return false;
		}

		return if (buffer.tryPush(v)) {

			readWaiter.exchange(null)?.succeedAsync(true);

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
			case DropWrite(f):
				f(v);

				return;
			case _:
				throw new Exception("Unsupported behaviour mode");
		}

		throw new ChannelClosedException();
	}

	@:coroutine public function waitForWrite():Bool {
		if (closed.get()) {
			return false;
		}

		return if (buffer.wasFull()) {
			suspendCancellable(cont -> {
				writeWaiter.store(cont);

				cont.onCancellationRequested = _ -> {
					writeWaiter.store(null);
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

		writeWaiter.exchange(null)?.succeedAsync(false);
		readWaiter.exchange(null)?.succeedAsync(false);
	}
}
