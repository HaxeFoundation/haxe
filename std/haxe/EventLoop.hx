package haxe;

import haxe.EntryPoint;

class Event {

	var prev : Event;
	var next : Event;
	var events : EventLoop;
	var callb : Void -> Void;
	/**
		The event priority. Events will be executed in order of priority (highest first).
	**/
	public var priority : Int;
	/**
		Tells if an event is blocking. It means the event loop won't return from `loop()` until this event has been stopped.
	**/
	public var isBlocking : Bool = true;
	var toRemove : Bool;
	var nextRun : Float = Math.NEGATIVE_INFINITY;

	function new(events, p) {
		this.events = events;
		this.priority = p;
	}

	/**
		Delay the execution of the event for the given time, in seconds.
		If t is null, the event will be run at next event loop.
	**/
	public function delay(t:Null<Float>,fromLastRun=false) {
		nextRun = t == null ? Math.NEGATIVE_INFINITY : (fromLastRun && Math.isFinite(nextRun) ? nextRun : haxe.Timer.stamp()) + t;
	}

	/**
		Stop this event from repeating.
	**/
	public function stop() {
		@:privateAccess events.stop(this);
	}

	/**
		Start the event with the given callback function.
	**/
	public function start( callb : Void -> Void ) {
		this.callb = callb;
		@:privateAccess events.start(this);
	}

	/**
		Tells if the event has been stopped.
	**/
	public function isStopped() {
		return toRemove || (prev == null && @:privateAccess events.events != this);
	}

}


/**
	Handles async events for all threads
**/
@:access(haxe.Event)
class EventLoop {

	/**
		This is the main thread event loop.
	**/
	public static var main(get,null) : EventLoop;

	/**
		This is the current thread event loop. For platforms that doesn't support threads
		it is the same as `main`.
	**/
	public static var current(get,never) : EventLoop;

	var events : Event;
	var inLoop : Bool;
	var hasPendingRemove : Bool;
	#if target.threaded
	var mutex : sys.thread.Mutex;
	var lockTime : sys.thread.Lock;
	#end

	public function new() {
		#if target.threaded
		mutex = new sys.thread.Mutex();
		lockTime = new sys.thread.Lock();
		#end
	}

	/**
		Runs until all the blocking events have been stopped.
		If this is called on the main thread, also wait for all blocking threads to finish.
	**/
	public function loop() {
		while( hasEvents(true) || (this == main && hasRunningThreads()) ) {
			var time = getNextTick();
			if( time > 0 ) {
				wait(time);
				continue;
			}
			loopOnce();
		}
	}

	inline function wakeup() {
		#if target.threaded
		lockTime.release();
		#end
	}

	inline function wait( time : Float ) {
		#if target.threaded
		lockTime.wait(time);
		#elseif sys
		Sys.sleep(time);
		#end
	}

	inline function lock() {
		#if target.threaded
		mutex.acquire();
		#end
	}

	inline function unlock() {
		#if target.threaded
		mutex.release();
		#end
	}

	/**
		Perform an update of pending events.
	**/
	public function loopOnce() {
		lock();
		sortEvents();
		var current = events; // protect from further add()
		inLoop = true;
		unlock();

		// if inLoop turns false, stop because we had reentrency
		var time = haxe.Timer.stamp();
		while( inLoop && current != null ) {
			var n = current.next;
			if( current.nextRun <= time && !current.toRemove )
				current.callb();
			current = n;
		}

		lock();
		inLoop = false;
		if( hasPendingRemove ) {
			hasPendingRemove = false;
			var e = events;
			while( e != null ) {
				var n = e.next;
				if( e.toRemove ) stop(e);
				e = n;
			}
		}
		unlock();
	}

	/**
		Add a callback to be run at each loop of the event loop.
	**/
	public function add( callb : Void -> Void, priority = 0 ) : Event {
		var e = new Event(this,priority);
		e.start(callb);
		return e;
	}

	/**
		Similar to `add` but will return the Event before it's started.
		This is useful if you wish to hold a reference of another thread Event loop
		before it runs.
	**/
	public function addAsync( priority = 0 ) {
		return new Event(this,priority);
	}

	/**
		Add a callback to be run every `delay` seconds until stopped
	**/
	public function addTimer( callb : Void -> Void, delay : Float, priority = 0 ) : Event {
		var e : Event = new Event(this,priority);
		e.delay(delay);
		e.start(function() { e.delay(delay,true); callb(); });
		return e;
	}

	@:deprecated @:noCompletion public function repeat( callb, delay : Int ) {
		return addTimer(callb,delay/1000);
	}

	@:deprecated @:noCompletion public function cancel( e : Event ) {
		e.stop();
	}

	/**
		Add a function to be run once at next loop of the event loop.
	**/
	public function run( callb : Void -> Void, priority = 0 ) : Event {
		var e : Event = new Event(this,priority);
		e.start(function() { e.stop(); callb(); });
		return e;
	}

	function start( e : Event ) {
		lock();
		e.toRemove = false;
		if( !e.isStopped() ) {
			unlock();
			return;
		}
		if( events != null )
			events.prev = e;
		e.next = events;
		events = e;
		wakeup();
		unlock();
	}

	function stop( e : Event ) {
		lock();
		if( inLoop ) {
			// prevent remove while in loopOnce()
			e.toRemove = true;
			hasPendingRemove = true;
			unlock();
			return;
		}
		e.toRemove = false;
		if( e.isStopped() ) {
			unlock();
			return;
		}
		if( events == e )
			events = e.next;
		else if( e.prev != null )
			e.prev.next = e.next;
		if( e.next != null ) {
			e.next.prev = e.prev;
			e.next = null;
		}
		e.prev = null;
		wakeup();
		unlock();
	}

	function getNextTick() : Float {
		lock();
		if( events == null ) {
			unlock();
			return 1e9;
		}
		var now = haxe.Timer.stamp();
		var e = events;
		var next = Math.POSITIVE_INFINITY;
		while( e != null ) {
			if( e.nextRun <= now ) {
				unlock();
				return -1;
			}
			if( e.nextRun < next )
				next = e.nextRun;
			e = e.next;
		}
		unlock();
		return next - now;
	}

	function sortEvents() {
		// pending = haxe.ds.ListSort.sort(pending, function(e1, e2) return e1.nextRun > e2.nextRun ? -1 : 1);
		// we can't use directly ListSort because it requires prev/next to be public, which we don't want here
		// we do then a manual inline, this also allow use to do a Float comparison of nextRun
		lock();
		var list = events;

		if (list == null) {
			unlock();
			return;
		}

		var insize = 1, nmerges, psize = 0, qsize = 0;
		var p, q, e, tail:Event;

		while (true) {
			p = list;
			list = null;
			tail = null;
			nmerges = 0;
			while (p != null) {
				nmerges++;
				q = p;
				psize = 0;
				for (i in 0...insize) {
					psize++;
					q = q.next;
					if (q == null)
						break;
				}
				qsize = insize;
				while (psize > 0 || (qsize > 0 && q != null)) {
					if (psize == 0) {
						e = q;
						q = q.next;
						qsize--;
					} else if (qsize == 0
						|| q == null
						|| (p.priority > q.priority || (p.priority == q.priority && p.nextRun <= q.nextRun))) {
						e = p;
						p = p.next;
						psize--;
					} else {
						e = q;
						q = q.next;
						qsize--;
					}
					if (tail != null)
						tail.next = e;
					else
						list = e;
					e.prev = tail;
					tail = e;
				}
				p = q;
			}
			tail.next = null;
			if (nmerges <= 1)
				break;
			insize *= 2;
		}
		list.prev = null; // not cycling
		events = list;
		unlock();
	}

	/**
		Tells if we currently have blocking unfinished threads.
	**/
	public static function hasRunningThreads() {
		#if !target.threaded
		return false;
		#else
		return @:privateAccess sys.thread.Thread.hasBlocking();
		#end
	}

	/**
		Tells if the event loop has remaining events.
		If blocking is set to true, only check if it has remaining blocking events.
	**/
	public function hasEvents( blocking : Bool = true ) {
		if( !blocking )
			return events != null;
		lock();
		var e = events;
		while( e != null ) {
			if( e.isBlocking ) {
				unlock();
				return true;
			}
			e = e.next;
		}
		unlock();
		return false;
	}

	/**
		Add a task to be run either on another thread or as part of the main event loop if the
		platform does not support threads.
	**/
	public static function addTask( f : Void -> Void, blocking = true ) {
		#if target.threaded
		sys.thread.Thread.create(f).isBlocking = blocking;
		#else
		main.add(f).isBlocking = blocking;
		#end
	}

	static function get_current() {
		#if target.threaded
		var events = sys.thread.Thread.current().events;
		if( events == null ) throw "The current thread doesn't have an event loop.";
		return events;
		#else
		return main;
		#end
	}

	static function get_main() {
		if( main == null ) main = new EventLoop();
		return main;
	}


}