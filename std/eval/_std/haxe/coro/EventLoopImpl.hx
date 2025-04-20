package haxe.coro;

import sys.thread.Mutex;

/**
	When an event loop has an available event to execute.
**/
private enum NextEventTime {
	/** There's already an event waiting to be executed */
	Now;
	/** No new events are expected. */
	Never;
	/** An event is expected to be ready for execution at `time`. */
	At(time:Float);
}

private class SimpleEventLoop {
	final mutex = new Mutex();
	final oneTimeEvents = new Array<Null<()->Void>>();
	var oneTimeEventsIdx = 0;
	var regularEvents:Null<RegularEvent>;

	public function new():Void {}

	/**
		Schedule event for execution every `intervalMs` milliseconds in current loop.
	**/
	public function repeat(event:()->Void, intervalMs:Int):EventHandler {
		var interval = 0.001 * intervalMs;
		var event = new RegularEvent(event, haxe.Timer.stamp() + interval, interval);
		mutex.acquire();
		insertEventByTime(event);
		mutex.release();
		return event;
	}

	function insertEventByTime(event:RegularEvent):Void {
		switch regularEvents {
			case null:
				regularEvents = event;
			case current:
				var previous = null;
				while(true) {
					if(current == null) {
						previous.next = event;
						event.previous = previous;
						break;
					} else if(event.nextRunTime < current.nextRunTime) {
						event.next = current;
						current.previous = event;
						switch previous {
							case null:
								regularEvents = event;
								case _:
								event.previous = previous;
								previous.next = event;
								current.previous = event;
						}
						break;
					} else {
						previous = current;
						current = current.next;
					}
				}
		}
	}

	/**
		Prevent execution of a previously scheduled event in current loop.
	**/
	public function cancel(eventHandler:EventHandler):Void {
		var event:RegularEvent = eventHandler;
		mutex.acquire();
		event.cancelled = true;
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
		event.next = event.previous = null;
		mutex.release();
	}

	/**
		Execute `event` as soon as possible.
	**/
	public function run(event:()->Void):Void {
		mutex.acquire();
		oneTimeEvents[oneTimeEventsIdx++] = event;
		mutex.release();
	}

	/**
		Executes all pending events.

		The returned time stamps can be used with `Sys.time()` for calculations.

		Depending on a target platform this method may be non-reentrant. It must
		not be called from event callbacks.
	**/
	public function progress():NextEventTime {
		return switch __progress(haxe.Timer.stamp(), [], []) {
			case -2: Now;
			case -1: Never;
			case time: At(time);
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
		var recycleRegular = [];
		var recycleOneTimers = [];
		while(true) {
			var r = __progress(haxe.Timer.stamp(), recycleRegular, recycleOneTimers);
			switch r {
				case -1:
				case -2:
					break;
				case time:
					var timeout = time - haxe.Timer.stamp();
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
	inline function __progress(now:Float, recycleRegular:Array<RegularEvent>, recycleOneTimers:Array<()->Void>):Float {
		var regularsToRun = recycleRegular;
		var eventsToRunIdx = 0;
		// When the next event is expected to run
		var nextEventAt:Float = -1;

		mutex.acquire();
		// Collect regular events to run
		var current = regularEvents;
		while(current != null) {
			if(current.nextRunTime <= now) {
				regularsToRun[eventsToRunIdx++] = current;
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
			if(!regularsToRun[i].cancelled)
				regularsToRun[i].run();
			regularsToRun[i] = null;
		}
		eventsToRunIdx = 0;

		var oneTimersToRun = recycleOneTimers;
		mutex.acquire();
		// Collect pending one-time events
		for(i => event in oneTimeEvents) {
			switch event {
				case null:
					break;
				case _:
					oneTimersToRun[eventsToRunIdx++] = event;
					oneTimeEvents[i] = null;
			}
		}
		oneTimeEventsIdx = 0;
		mutex.release();

		//run events
		for(i in 0...eventsToRunIdx) {
			oneTimersToRun[i]();
			oneTimersToRun[i] = null;
		}

		// Some events were executed. They could add new events to run.
		if(eventsToRunIdx > 0) {
			nextEventAt = -2;
		}
		return nextEventAt;
	}
}

abstract EventHandler(RegularEvent) from RegularEvent to RegularEvent {}

private class RegularEvent {
	public var nextRunTime:Float;
	public final interval:Float;
	public final run:()->Void;
	public var next:Null<RegularEvent>;
	public var previous:Null<RegularEvent>;
	public var cancelled:Bool = false;

	public function new(run:()->Void, nextRunTime:Float, interval:Float) {
		this.run = run;
		this.nextRunTime = nextRunTime;
		this.interval = interval;
	}
}

typedef EventLoopImpl = SimpleEventLoop;