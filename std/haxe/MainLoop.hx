package haxe;
import haxe.EntryPoint;

class MainEvent {

	var f : Void -> Void;
	var prev : MainEvent;
	var next : MainEvent;
	public var nextRun(default,null) : Null<Float>;
	public var priority(default,null) : Int;

	function new(f,p) {
		this.f = f;
		this.priority = p;
	}

	/**
		Delay the execution of the event for the given time, in seconds.
		If t is null, the event will be run at tick() time.
	**/
	public function delay( t : Null<Float> ) {
		nextRun = t == null ? null : haxe.Timer.stamp() + t;
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
		f = null;
		nextRun = null;
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

	public static function hasEvents() {
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
		var e = new MainEvent(f,priority);
		var head = pending;
		if( head == null ) {
			pending = e;
			return e;
		}
		var prev = null;
		while( head != null && head.priority > priority ) {
			prev = head;
			head = head.next;
		}
		if( prev == null ) {
			e.next = head;
			head.prev = e;
			pending = e;
		} else {
			var n = prev.next;
			if( n != null ) n.prev = e;
			e.next = n;
			prev.next = e;
			e.prev = prev;
		}
		return e;
	}

	/**
		Run the pending events. Return the time for next event.
	**/
	static function tick() {
		// TODO : it will be necessary to reorder the events based on nextRun before
		//		  processing them so they get run in correct order. this can be done in MainEvent.delay() as well but might
		//		  cause ordering issues since we're processing events here
		var e = pending;
		var now = haxe.Timer.stamp();
		var wait = 1e9;
		while( e != null ) {
			var next = e.next;
			if( e.nextRun == null ) {
				wait = 0;
				e.call();
			} else {
				var wt = e.nextRun - now;
				if( wt <= 0 ) {
					wait = 0;
					e.call();
				} else if( wait > wt )
					wait = wt;
			}
			e = next;
		}
		return wait;
	}

}