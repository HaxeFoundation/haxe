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
	final closeClosure : CloseClosure;

	public function new() {
		super();

		first        = null;
		last         = null;
		noOpHandle   = new NoOpHandle();
		zeroEvents   = new DoubleBuffer();
		closeClosure = close;
	}

    public function schedule(ms:Int, func:()->Void):ISchedulerHandle {
		if (ms < 0) {
			throw new ArgumentException("Time must be greater or equal to zero");
		} else if (ms == 0) {
			zeroEvents.push(func);
			return noOpHandle;
		}

		final event = new ScheduledEvent(closeClosure, func, now() + (ms / 1000));
		if (first == null) {
			first = event;
			last = event;
			return event;
		}

		var current = last;
		while (true) {
			if (current == null) {
				event.next = first;
				first = event;
				return event;
			} else if (event.runTime >= current.runTime) {
				final next = current.next;
				current.next = event;
				event.previous = current;
				if (next != null) {
					event.next = next;
					next.previous = event;
				} else {
					last = event;
				}
				return event;
			} else {
				current = current.previous;
			}
		}
    }

	public function now() {
		return Timer.stamp();
	}

	public function run() {

		while (true) {
			for (event in zeroEvents.flip()) {
				event();
			}

			final currentTime = now();
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
			if (zeroEvents.empty()) {
				return;
			}
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