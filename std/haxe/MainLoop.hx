package haxe;
import haxe.EntryPoint;

class MainEvent {

	var f : Void -> Void;
	var prev : MainEvent;
	var next : MainEvent;
	public var nextRun(default,null) : Float;
	public var priority(default,null) : Int;

	function new(f, p) {
		this.f = f;
		this.priority = p;
		nextRun = -1;
	}

	/**
		Delay the execution of the event for the given time, in seconds.
		If t is null, the event will be run at tick() time.
	**/
	public function delay( t : Null<Float> ) {
		nextRun = t == null ? -1 : haxe.Timer.stamp() + t;
	}

	/**
		Call the event. Will do nothing is the event has been stopped.
	**/
	public inline function call() {
		if( f != null ) f();
	}

	/**
		Stop the event from firing anymore.
	**/
	public function stop() {
		if( f == null ) return;
		f = null;
		nextRun = -1;
		if( prev == null )
			@:privateAccess MainLoop.pending = next;
		else
			prev.next = next;
		if( next != null )
			next.prev = prev;
	}

}

@:access(haxe.MainEvent)
class MainLoop {

	static var pending : MainEvent = null;

	public static var threadCount(get, never) : Int;

	inline static function get_threadCount() return EntryPoint.threadCount;

	public inline static function hasEvents() {
		return pending != null;
	}

	public static function addThread( f : Void -> Void ) {
		EntryPoint.addThread(f);
	}

	public static function runInMainThread( f : Void -> Void ) {
		EntryPoint.runInMainThread(f);
	}

	/**
		Add a pending event to be run into the main loop.
	**/
	public static function add( f : Void -> Void, priority = 0 ) : MainEvent @:privateAccess {
		if( f == null )
			throw "Event function is null";
		var e = new MainEvent(f, priority);
		var head = pending;
		if( head != null ) head.prev = e;
		e.next = head;
		pending = e;
		return e;
	}

	static function sortEvents() {
		// pending = haxe.ds.ListSort.sort(pending, function(e1, e2) return e1.nextRun > e2.nextRun ? -1 : 1);
		// we can't use directly ListSort because it requires prev/next to be public, which we don't want here
		// we do then a manual inline, this also allow use to do a Float comparison of nextRun
		var list = pending;

		if( list == null ) return;

		var insize = 1, nmerges, psize = 0, qsize = 0;
		var p, q, e, tail : MainEvent;

		while( true ) {
			p = list;
			list = null;
			tail = null;
			nmerges = 0;
			while( p != null ) {
				nmerges++;
				q = p;
				psize = 0;
				for( i in 0...insize ) {
					psize++;
					q = q.next;
					if( q == null ) break;
				}
				qsize = insize;
				while( psize > 0 || (qsize > 0 && q != null) ) {
					if( psize == 0 ) {
						e = q;
						q = q.next;
						qsize--;
					} else if( qsize == 0 || q == null || (p.priority > q.priority || (p.priority == q.priority && p.nextRun <= q.nextRun)) ) {
						e = p;
						p = p.next;
						psize--;
					} else {
						e = q;
						q = q.next;
						qsize--;
					}
					if( tail != null )
						tail.next = e;
					else
						list = e;
					e.prev = tail;
					tail = e;
				}
				p = q;
			}
			tail.next = null;
			if( nmerges <= 1 )
				break;
			insize *= 2;
		}
		list.prev = null; // not cycling
		pending = list;
	}

	/**
		Run the pending events. Return the time for next event.
	**/
	static function tick() {
		sortEvents();
		var e = pending;
		var now = haxe.Timer.stamp();
		var wait = 1e9;
		while( e != null ) {
			var next = e.next;
			var wt = e.nextRun - now;
			if( e.nextRun < 0 || wt <= 0 ) {
				wait = 0;
				e.call();
			} else if( wait > wt )
				wait = wt;
			e = next;
		}
		return wait;
	}

}