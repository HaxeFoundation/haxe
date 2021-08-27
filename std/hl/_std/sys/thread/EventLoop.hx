package sys.thread;

import hl.I64;
import hl.uv.Loop;
import hl.uv.Async;
import hl.uv.Timer as UVTimer;

@:coreApi
enum NextEventTime {
	Now;
	Never;
	AnyTime(time:Null<Float>);
	At(time:Float);
}

abstract EventHandler(RegularEvent) from RegularEvent to RegularEvent {}

private class RegularEvent {
	public var timer:Null<UVTimer>;
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
	@:allow(hl.uv.Loop)
	final handle:Loop;

	final mutex = new Mutex();
	final oneTimeEvents = new Array<Null<()->Void>>();
	var oneTimeEventsIdx = 0;
	final wakeup:Async;
	var promisedEventsCount = 0;
	var pending:Array<()->Void> = [];
	var looping = false;

	public function new():Void {
		handle = Loop.init();
		wakeup = Async.init(handle, consumePending);
		wakeup.unref();
	}

	public function repeat(event:()->Void, intervalMs:Int):EventHandler {
		var e = new RegularEvent(event);
		mutex.acquire();
		pending.push(() -> {
			e.timer = UVTimer.init(handle);
			e.timer.start(e.run, I64.ofInt(intervalMs), I64.ofInt(intervalMs < 1 ? 1 : intervalMs));
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
			if(!timer.isClosing()) {
				timer.stop();
				timer.close(noop);
			}
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
		if((handle:Loop).run(NoWait)) {
			return AnyTime(null);
		} else {
			return Never;
		}
	}

	public function wait(?timeout:Float):Bool {
		//TODO: throw if loop is already running
		if(timeout == null) {
			var timer = UVTimer.init(handle);
			timer.start(() -> {
				timer.stop();
				timer.close();
			}, I64.ofInt(Std.int(timeout * 1000)), I64.ofInt(0));
			return (handle:Loop).run(Once);
		} else {
			return (handle:Loop).run(Once);
		}
	}

	public function loop():Void {
		//TODO: throw if loop is already running
		consumePending();
		(handle:Loop).run(Default);
	}

	function consumePending(?_:Async):Void {
		var p = pending;
		pending = [];
		for(fn in p) fn();
	}
}