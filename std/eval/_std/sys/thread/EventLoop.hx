package sys.thread;

import eval.luv.Loop;
import eval.luv.Async;
import eval.luv.Timer as LuvTimer;
import haxe.MainLoop;

/**
	When an event loop has an available event to execute.
**/
@:coreApi
enum NextEventTime {
	/** There's already an event waiting to be executed */
	Now;
	/** No new events are expected. */
	Never;
	/**
		An event is expected to arrive at any time.
		If `time` is specified, then the event will be ready at that time for sure.
	*/
	AnyTime(time:Null<Float>);
	/** An event is expected to be ready for execution at `time`. */
	At(time:Float);
}

abstract EventHandler(RegularEvent) from RegularEvent to RegularEvent {}

private class RegularEvent {
	public var timer:Null<LuvTimer>;
	public var event:()->Void;

	public function new(e:()->Void) {
		event = e;
	}

	public function run() {
		event();
	}
}

/**
	An event loop implementation used for `sys.thread.Thread`
**/
@:coreApi
class EventLoop {
	@:allow(eval.luv.Loop)
	final handle:Loop;

	final mutex = new Mutex();
	final wakeup:Async;
	var promisedEventsCount = 0;
	var pending:Array<()->Void> = [];
	var started:Bool = false;

	var isMainThread:Bool;
	static var CREATED : Bool;

	public function new():Void {
		isMainThread = !CREATED;
		CREATED = true;
		handle = Loop.init().resolve();
		wakeup = Async.init(handle, consumePending).resolve();
		wakeup.unref();
	}

	/**
		Schedule event for execution every `intervalMs` milliseconds in current loop.
	**/
	public function repeat(event:()->Void, intervalMs:Int):EventHandler {
		var e = new RegularEvent(event);
		mutex.acquire();
		e.timer = LuvTimer.init(handle).resolve();
		e.timer.start(e.run, intervalMs, intervalMs < 1 ? 1 : intervalMs).resolve();
		mutex.release();
		wakeup.send();
		return e;
	}

	/**
		Prevent execution of a previously scheduled event in current loop.
	**/
	public function cancel(eventHandler:EventHandler):Void {
		mutex.acquire();
		(eventHandler:RegularEvent).event = noop;
		pending.push(() -> {
			var timer = (eventHandler:RegularEvent).timer;
			timer.stop().resolve();
			timer.close(noop);
		});
		mutex.release();
		wakeup.send();
	}
	static final noop = function() {}

	/**
		Notify this loop about an upcoming event.
		This makes the thread stay alive and wait for as many events as the number of
		times `.promise()` was called. These events should be added via `.runPromised()`.
	**/
	public function promise():Void {
		mutex.acquire();
		++promisedEventsCount;
		pending.push(refUnref);
		mutex.release();
		wakeup.send();
	}

	/**
		Execute `event` as soon as possible.
	**/
	public function run(event:()->Void):Void {
		mutex.acquire();
		pending.push(event);
		mutex.release();
		wakeup.send();
	}

	/**
		Add previously promised `event` for execution.
	**/
	public function runPromised(event:()->Void):Void {
		mutex.acquire();
		--promisedEventsCount;
		pending.push(refUnref);
		pending.push(event);
		mutex.release();
		wakeup.send();
	}

	function refUnref():Void {
		if (promisedEventsCount > 0 || (isMainThread && haxe.MainLoop.hasEvents())) {
			wakeup.ref();
		} else {
			wakeup.unref();
		}
	}

	public function progress():NextEventTime {
		if (started) throw "Event loop already started";

		if (handle.run(NOWAIT)) {
			return AnyTime(null);
		} else {
			return Never;
		}
	}

	/**
		Blocks until a new event is added or `timeout` (in seconds) to expires.

		Depending on a target platform this method may also automatically execute arriving
		events while waiting. However if any event is executed it will stop waiting.

		Returns `true` if more events are expected.
		Returns `false` if no more events expected.

		Depending on a target platform this method may be non-reentrant. It must
		not be called from event callbacks.
	**/
	public function wait(?timeout:Float):Bool {
		if (started) throw "Event loop already started";

		if(timeout != null) {
			var timer = LuvTimer.init(handle).resolve();
			timer.start(() -> {
				timer.stop().resolve();
				timer.close(() -> {});
			}, Std.int(timeout * 1000));
			return handle.run(ONCE);
		} else {
			return handle.run(ONCE);
		}
	}

	/**
		Execute all pending events.
		Wait and execute as many events as the number of times `promise()` was called.
		Runs until all repeating events are cancelled and no more events are expected.

		Depending on a target platform this method may be non-reentrant. It must
		not be called from event callbacks.
	**/
	public function loop():Void {
		if (started) throw "Event loop already started";
		started = true;
		consumePending();
		handle.run(DEFAULT);
	}

	function consumePending(?_:Async):Void {
		mutex.acquire();
		var p = pending;
		pending = [];
		mutex.release();
		for(fn in p) fn();

		if (started && isMainThread) {
			var next = @:privateAccess MainLoop.tick();
			if (haxe.MainLoop.hasEvents()) wakeup.send();
			refUnref();
		}
	}
}
