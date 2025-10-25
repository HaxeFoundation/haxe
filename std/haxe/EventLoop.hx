package haxe;

import haxe.EntryPoint;

class Event {

	var prev : Event;
	var next : Event;
	var events : EventLoop;
	var callb : Void -> Void;
	public var priority : Int;
	public var isBlocking : Bool = true;
	var toRemove : Bool;
	var nextRun : Float = Math.NEGATIVE_INFINITY;

	function new(events, callb, p) {
		this.events = events;
		this.callb = callb;
		this.priority = p;
	}

	/**
		Delay the execution of the event for the given time, in seconds.
		If t is null, the event will be run at next event loop.
	**/
	public function delay(t:Null<Float>,fromLastRun=false) {
		nextRun = t == null ? Math.NEGATIVE_INFINITY : (fromLastRun && Math.isFinite(nextRun) ? nextRun : haxe.Timer.stamp()) + t;
	}

	public function stop() {
		@:privateAccess events.remove(this);
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
		#else
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
				if( e.toRemove ) remove(e);
				e = n;
			}
		}
		unlock();
	}

	/**
		Add a callback to be run at each loop of the event loop.
	**/
	public function add( callb : Void -> Void, priority = 0 ) : Event {
		var e = new Event(this,callb,priority);
		lock();
		if( events != null )
			events.prev = e;
		e.next = events;
		events = e;
		wakeup();
		unlock();
		return e;
	}

	/**
		Add a callback to be run every `delay` seconds until stopped
	**/
	public function addTimer( callb : Void -> Void, delay : Float, priority = 0 ) : Event {
		var e : Event = null;
		e = new Event(this,function() { e.delay(delay,true); callb(); },priority);
		e.delay(delay);
		lock();
		if( events != null )
			events.prev = e;
		e.next = events;
		events = e;
		wakeup();
		unlock();
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
		var e : Event = null;
		e = add(function() { e.stop(); callb(); }, priority);
		return e;
	}

	function remove( e : Event ) {
		lock();
		if( inLoop ) {
			// prevent remove while in loopOnce()
			e.toRemove = true;
			hasPendingRemove = true;
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

	public static function hasRunningThreads() {
		#if !target.threaded
		return false;
		#else
		return @:privateAccess sys.thread.Thread.hasBlocking();
		#end
	}

	/**
		Tells if the event loop has remaining events
	**/
	public function hasEvents( blocking : Bool ) {
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