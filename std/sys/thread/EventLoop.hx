package sys.thread;

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

/**
	An event loop implementation used for `sys.thread.Thread`
**/
@:coreApi
class EventLoop {
	final mutex = new Mutex();
	final oneTimeEvents = new Array<Null<()->Void>>();
	var oneTimeEventsIdx = 0;
	final waitLock = new Lock();
	var promisedEventsCount = 0;
	var regularEvents:Null<RegularEvent>;

	public function new():Void {}

	/**
		Schedule event for execution every `intervalMs` milliseconds in current loop.
	**/
	public function repeat(event:()->Void, intervalMs:Int):EventHandler {
		mutex.acquire();
		var interval = 0.001 * intervalMs;
		var event = new RegularEvent(event, Sys.time() + interval, interval);
		switch regularEvents {
			case null:
			case current:
				event.next = current;
				current.previous = event;
		}
		regularEvents = event;
		waitLock.release();
		mutex.release();
		return event;
	}

	/**
		Prevent execution of a previously scheduled event in current loop.
	**/
	public function cancel(eventHandler:EventHandler):Void {
		mutex.acquire();
		var event:RegularEvent = eventHandler;
		if(regularEvents == event) {
			regularEvents = event.next;
		}
		switch event.next {
			case null:
			case e: e.previous = event.previous;
		}
		switch event.previous {
			case null:
			case e: e.next = event.next;
		}
		mutex.release();
	}

	/**
		Notify this loop about an upcoming event.
		This makes the thread to stay alive and wait for as many events as many times
		`.promise()` was called. These events should be added via `.runPromised()`
	**/
	public function promise():Void {
		mutex.acquire();
		++promisedEventsCount;
		mutex.release();
	}

	/**
		Execute `event` as soon as possible.
	**/
	public function run(event:()->Void):Void {
		mutex.acquire();
		oneTimeEvents[oneTimeEventsIdx++] = event;
		waitLock.release();
		mutex.release();
	}

	/**
		Add previously promised `event` for execution.
	**/
	public function runPromised(event:()->Void):Void {
		mutex.acquire();
		oneTimeEvents[oneTimeEventsIdx++] = event;
		--promisedEventsCount;
		waitLock.release();
		mutex.release();
	}

	/**
		Executes all pending events.

		The returned time stamps can be used with `Sys.time()` for calculations.

		Depending on a target platform this method may be non-reentrant. It must
		not be called from event callbacks.
	**/
	public function progress():NextEventTime {
		return switch __progress(Sys.time(), []) {
			case {nextEventAt:-2}: Now;
			case {nextEventAt:-1, anyTime:false}: Never;
			case {nextEventAt:-1, anyTime:true}: AnyTime(null);
			case {nextEventAt:time, anyTime:true}: AnyTime(time);
			case {nextEventAt:time, anyTime:false}: At(time);
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
		return waitLock.wait(timeout);
	}

	/**
		Execute all pending events.
		Wait and execute as many events as many times `promiseEvent()` was called.
		Runs until all repeating events are cancelled and no more events is expected.

		Depending on a target platform this method may be non-reentrant. It must
		not be called from event callbacks.
	**/
	public function loop():Void {
		var events = [];
		while(true) {
			var r = __progress(Sys.time(), events);
			switch r {
				case {nextEventAt:-2}:
				case {nextEventAt:-1, anyTime:false}:
					break;
				case {nextEventAt:-1, anyTime:true}:
					waitLock.wait();
				case {nextEventAt:time}:
					var timeout = time - Sys.time();
					waitLock.wait(Math.max(0, timeout));
			}
		}
	}

	/**
		`.progress` implementation with a reusable array for internal usage.
		The `nextEventAt` field of the return value denotes when the next event
		is expected to run:
		* -1 - never
		* -2 - now
		* other values - at specified time
	**/
	inline function __progress(now:Float, recycle:Array<()->Void>):{nextEventAt:Float, anyTime:Bool} {
		var eventsToRun = recycle;
		var eventsToRunIdx = 0;
		// When the next event is expected to run
		var nextEventAt:Float = -1;

		mutex.acquire();
		//reset waitLock
		while(waitLock.wait(0.0)) {}
		// Collect regular events to run
		var current = regularEvents;
		while(current != null) {
			if(current.nextRunTime <= now) {
				eventsToRun[eventsToRunIdx++] = current.run;
				current.nextRunTime += current.interval;
				nextEventAt = -2;
			} else if(nextEventAt == -1 || current.nextRunTime < nextEventAt) {
				nextEventAt = current.nextRunTime;
			}
			current = current.next;
		}
		mutex.release();

		// Run regular events
		for(i in 0...eventsToRunIdx) {
			eventsToRun[i]();
			eventsToRun[i] = null;
		}
		eventsToRunIdx = 0;

		// Collect pending one-time events
		mutex.acquire();
		for(i => event in oneTimeEvents) {
			switch event {
				case null:
					break;
				case _:
					eventsToRun[eventsToRunIdx++] = event;
					oneTimeEvents[i] = null;
			}
		}
		oneTimeEventsIdx = 0;
		var hasPromisedEvents = promisedEventsCount > 0;
		mutex.release();

		//run events
		for(i in 0...eventsToRunIdx) {
			eventsToRun[i]();
			eventsToRun[i] = null;
		}

		// Some events were executed. They could add new events to run.
		if(eventsToRunIdx > 0) {
			nextEventAt = -2;
		}
		return {nextEventAt:nextEventAt, anyTime:hasPromisedEvents}
	}
}

abstract EventHandler(RegularEvent) from RegularEvent to RegularEvent {}

private class RegularEvent {
	public var nextRunTime:Float;
	public final interval:Float;
	public final run:()->Void;
	public var next:Null<RegularEvent>;
	public var previous:Null<RegularEvent>;

	public function new(run:()->Void, nextRunTime:Float, interval:Float) {
		this.run = run;
		this.nextRunTime = nextRunTime;
		this.interval = interval;
	}
}