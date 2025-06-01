package haxe.coro.schedulers;

import haxe.exceptions.ArgumentException;

private typedef Lambda = ()->Void;
private typedef CloseClosure = (handle:ISchedulerHandle)->Void;

private class ScheduledEvent implements ISchedulerHandle {
	final closure : CloseClosure;
	final func : Lambda;
	var closed : Bool;
	public final runTime : Float;
	public var next : Null<ScheduledEvent>;
	public var previous : Null<ScheduledEvent>;

	public function new(closure, func, runTime) {
		this.closure = closure;
		this.func    = func;
		this.runTime = runTime;

		closed   = false;
		next     = null;
		previous = null;
	}

	public inline function run() {
		func();

		closed = true;
	}

	public function close() {
		if (closed) {
			return;
		}

		closure(this);

		closed = true;
	}
}

private class NoOpHandle implements ISchedulerHandle {
	public function new() {}
	public function close() {}
}

private class DoubleBuffer {
	final a : Array<Lambda>;
	final b : Array<Lambda>;

	var current : Array<Lambda>;

	public function new() {
		a       = [];
		b       = [];
		current = a;
	}

	public function flip() {
		final returning = current;

		current = if (current == a) b else a;
		current.resize(0);

		return returning;
	}

	public function push(l : Lambda) {
		current.push(l);
	}

	public function empty() {
		return current.length == 0;
	}
}

class EventLoopScheduler extends Scheduler {
	var first : Null<ScheduledEvent>;
	var last : Null<ScheduledEvent>;

	final noOpHandle : NoOpHandle;
	final zeroEvents : DoubleBuffer;
	final zeroMutex : Mutex;
	final futureMutex : Mutex;
	final closeClosure : CloseClosure;

	public function new() {
		super();

		first        = null;
		last         = null;
		noOpHandle   = new NoOpHandle();
		zeroEvents   = new DoubleBuffer();
		zeroMutex    = new Mutex();
		futureMutex  = new Mutex();
		closeClosure = close;
	}

    public function schedule(ms:Int, func:()->Void):ISchedulerHandle {
		if (ms < 0) {
			throw new ArgumentException("Time must be greater or equal to zero");
		} else if (ms == 0) {
			zeroMutex.acquire();
			zeroEvents.push(func);
			zeroMutex.release();
			return noOpHandle;
		}

		final event = new ScheduledEvent(closeClosure, func, now() + (ms / 1000));

		futureMutex.acquire();
		if (first == null) {
			first = event;
			last = event;
			futureMutex.release();
			return event;
		}

		var currentLast = last;
		var currentFirst = first;
		while (true) {
			if (event.runTime >= currentLast.runTime) {
				final next = currentLast.next;
				currentLast.next = event;
				event.previous = currentLast;
				if (next != null) {
					event.next = next;
					next.previous = event;
				} else {
					last = event;
				}
				futureMutex.release();
				return event;
			}
			else if (event.runTime < currentFirst.runTime) {
				final previous = currentFirst.previous;
				currentFirst.previous = event;
				event.next = currentFirst;
				if (previous != null) {
					event.previous = previous;
					previous.next = event;
				} else {
					first = event;
				}
				futureMutex.release();
				return event;
			} else {
				currentFirst = currentLast.next;
				currentLast = currentLast.previous;
				// if one of them is null, set to the other so the next iteration will definitely
				// hit one of the two branches above
				if (currentFirst == null) {
					currentFirst = currentLast;
				} else if (currentLast == null) {
					currentLast = currentFirst;
				}
			}
		}
    }

	public function now() {
		return Timer.stamp();
	}

	public function run() {

		while (true) {
			zeroMutex.acquire();
			for (event in zeroEvents.flip()) {
				event();
			}
			zeroMutex.release();

			final currentTime = now();

			futureMutex.acquire();
			while (true) {
				if (first == null) {
					last = null;
					break;
				}
				if (first.runTime <= currentTime) {
					final toRun = first;
					first = first.next;
					if (first != null) {
						first.previous = null;
					}
					toRun.run();
				} else {
					break;
				}
			}
			futureMutex.release();

			zeroMutex.acquire();
			if (zeroEvents.empty()) {
				zeroMutex.release();
				return;
			}
			zeroMutex.release();
		}
	}

	public function toString() {
		return '[EventLoopScheduler]';
	}

	function close(handle : ISchedulerHandle) {
		var current = first;
		while (true) {
			if (null == current) {
				return;
			}

			if (current == handle) {
				if (first == current) {
					first = current.next;
				} else {
					final a = current.previous;
					final b = current.next;
	
					a.next = b;
					b.previous = a;
				}

				return;
			} else {
				current = current.next;
			}
		}
	}
}