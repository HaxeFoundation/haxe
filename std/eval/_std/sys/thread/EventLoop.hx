package sys.thread;

import eval.luv.Loop;
import eval.luv.Async;
import eval.luv.Timer as LuvTimer;
import haxe.MainLoop;

@:coreApi
enum NextEventTime {
	Now;
	Never;
	AnyTime(time:Null<Float>);
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

@:coreApi
class EventLoop {
	@:allow(eval.luv.Loop)
	final handle:Loop;

	final mutex = new Mutex();
	final oneTimeEvents = new Array<Null<()->Void>>();
	var oneTimeEventsIdx = 0;
	final wakeup:Async;
	var promisedEventsCount = 0;
	var pending:Array<()->Void> = [];
	var looping = false;

	var isMainThread:Bool;
	static var CREATED : Bool;

	public function new():Void {
		isMainThread = !CREATED;
		CREATED = true;
		handle = Loop.init().resolve();
		wakeup = Async.init(handle, consumePending).resolve();
		wakeup.unref();
	}

	public function repeat(event:()->Void, intervalMs:Int):EventHandler {
		var e = new RegularEvent(event);
		mutex.acquire();
		pending.push(() -> {
			e.timer = LuvTimer.init(handle).resolve();
			e.timer.start(e.run, intervalMs, intervalMs < 1 ? 1 : intervalMs).resolve();
		});
		mutex.release();
		wakeup.send();
		return e;
	}

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

	public function promise():Void {
		mutex.acquire();
		++promisedEventsCount;
		pending.push(refUnref);
		mutex.release();
		wakeup.send();
	}

	public function run(event:()->Void):Void {
		mutex.acquire();
		pending.push(event);
		mutex.release();
		wakeup.send();
	}

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

	public function progress():NextEventTime {
		//TODO: throw if loop is already running
		if((handle:Loop).run(NOWAIT)) {
			return AnyTime(null);
		} else {
			return Never;
		}
	}

	public function wait(?timeout:Float):Bool {
		//TODO: throw if loop is already running
		if(timeout == null) {
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

	public function loop():Void {
		//TODO: throw if loop is already running
		consumePending();
		handle.run(DEFAULT);
	}

	function consumePending(?_:Async):Void {
		var p = pending;
		pending = [];
		for(fn in p) fn();
		if (isMainThread && MainLoop.hasEvents()) {
			runPromised(() -> @:privateAccess MainLoop.tick());
		}
	}
}
