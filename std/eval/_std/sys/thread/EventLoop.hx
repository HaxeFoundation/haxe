package sys.thread;

import eval.luv.Loop;
import eval.luv.Async;
import eval.luv.Timer as LuvTimer;

/**
	When an event loop has an available event to execute.
**/
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

abstract EventLoopHandle(Loop) from Loop to Loop {}

abstract EventHandler(RegularEvent) from RegularEvent to RegularEvent {}

private class RegularEvent {
	public var timer:Null<LuvTimer>;
	public function new() {}
}

/**
	An event loop implementation used for `sys.thread.Thread`
**/
@:coreApi
class EventLoop {
	public final handle:EventLoopHandle;

	final mutex = new Mutex();
	final oneTimeEvents = new Array<Null<()->Void>>();
	var oneTimeEventsIdx = 0;
	final wakeup:Async;
	var promisedEventsCount = 0;
	var pending:Array<()->Void> = [];

	public function new():Void {
		handle = Loop.init().resolve();
		wakeup = Async.init(handle, consumePending).resolve();
		wakeup.unref();
	}

	/**
		Schedule event for execution every `intervalMs` milliseconds in current loop.
	**/
	public function repeat(event:()->Void, intervalMs:Int):EventHandler {
		var e = new RegularEvent();
		mutex.acquire();
		pending.push(() -> {
			e.timer = LuvTimer.init(handle).resolve();
			e.timer.start(event, intervalMs, intervalMs < 1 ? 1 : intervalMs).resolve();
		});
		mutex.release();
		wakeup.send();
		return e;
	}

	/**
		Prevent execution of a previousely scheduled event in current loop.
	**/
	public function cancel(eventHandler:EventHandler):Void {
		mutex.acquire();
		pending.push(() -> {
			var timer = (eventHandler:RegularEvent).timer;
			timer.stop().resolve();
			timer.close(() -> {});
		});
		mutex.release();
		wakeup.send();
	}

	/**
		Notify this loop about an upcoming event.
		This makes the thread to stay alive and wait for as many events as many times
		`.promise()` was called. These events should be added via `.runPromised()`
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
		if(promisedEventsCount > 0) {
			wakeup.ref();
		} else {
			wakeup.unref();
		}
	}

	/**
		Executes all pending events.

		The returned time stamps can be used with `Sys.time()` for calculations.
	**/
	public function progress():NextEventTime {
		if((handle:Loop).run(NOWAIT)) {
			return AnyTime(null);
		} else {
			return Never;
		}
	}

	/**
		Waits for a new event to be added, or `timeout` (in seconds) to expire.
		Returns `true` if an event was added and `false` if a timeout occurs.
	**/
	public function wait(?timeout:Float):Bool {
		throw new haxe.exceptions.NotImplementedException();
	}

	/**
		Execute all pending events.
		Wait and execute as many events as many times `promiseEvent()` was called.
		Runs until all repeating events are cancelled and no more events is expected.
	**/
	public function loop():Void {
		//TODO: throw if loop is already running
		consumePending();
		(handle:Loop).run(DEFAULT);
	}

	function consumePending(?_:Async):Void {
		var p = pending;
		pending = [];
		for(fn in p) fn();
	}
}