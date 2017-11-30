package haxe;

#if (neko && !macro && !interp)
import neko.vm.Lock;
import neko.vm.Mutex;
import neko.vm.Thread;
#elseif cpp
import cpp.vm.Lock;
import cpp.vm.Mutex;
import cpp.vm.Thread;
#elseif java
import java.vm.Lock;
import java.vm.Mutex;
import java.vm.Thread;
#elseif sys
private class Lock {
	public function new() {
	}
	public inline function release() {
	}
	public inline function wait( ?t : Float ) {
	}
}
private class Mutex {
	public function new() {
	}
	public inline function acquire() {
	}
	public inline function release() {
	}
}
private class Thread {
	public static function create( f : Void -> Void ) {
		f();
	}
}
#end

/**
	If haxe.MainLoop is kept from DCE, then we will insert an haxe.EntryPoint.run() call just at then end of main().
	This class can be redefined by custom frameworks so they can handle their own main loop logic.
**/
class EntryPoint {

	#if sys
	static var sleepLock = new Lock();
	static var mutex = new Mutex();
	#end
	static var pending = new Array<Void->Void>();

	public static var threadCount(default,null) : Int = 0;

	/**
		Wakeup a sleeping run()
	**/
	public static function wakeup() {
		#if sys
		sleepLock.release();
		#end
	}

	public static function runInMainThread( f : Void -> Void ) {
		#if sys
		mutex.acquire();
		pending.push(f);
		mutex.release();
		wakeup();
		#else
		pending.push(f);
		#end
	}

	public static function addThread( f : Void -> Void ) {
		#if sys
		mutex.acquire();
		threadCount++;
		mutex.release();
		Thread.create(function() {
			f();
			mutex.acquire();
			threadCount--;
			if( threadCount == 0 ) wakeup();
			mutex.release();
		});
		#else
		threadCount++;
		pending.push(function() { f(); threadCount--; } );
		#end
	}

	static function processEvents() : Float {
		// flush all pending calls
		while( true ) {
			#if sys
			mutex.acquire();
			var f = pending.shift();
			mutex.release();
			#else
			var f = pending.shift();
			#end
			if( f == null ) break;
			f();
		}
		if( !MainLoop.hasEvents() && threadCount == 0 )
			return -1;
		return @:privateAccess MainLoop.tick();
	}

	/**
		Start the main loop. Depending on the platform, this can return immediately or will only return when the application exits.
	**/
	@:keep public static function run() @:privateAccess {
		#if js

		var nextTick = processEvents();

		#if nodejs
		if( nextTick < 0 )
			return;
		(untyped setTimeout)(run,nextTick);
		#else
		var window : Dynamic = js.Browser.window;
		var rqf : Dynamic = window.requestAnimationFrame ||
			window.webkitRequestAnimationFrame ||
			window.mozRequestAnimationFrame;
		rqf(run);
		#end

		#elseif flash

		flash.Lib.current.stage.addEventListener(flash.events.Event.ENTER_FRAME, function(_) processEvents());

		#elseif sys
		while( true ) {
			var nextTick = processEvents();
			if( nextTick < 0 )
				break;
			if( nextTick > 0 )
				sleepLock.wait(nextTick); // wait until nextTick or wakeup() call
		}
		#else

		// no implementation available, let's exit immediately

		#end
	}
}
