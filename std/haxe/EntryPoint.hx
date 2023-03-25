package haxe;

#if (target.threaded && !cppia)
import sys.thread.Lock;
import sys.thread.Mutex;
import sys.thread.Thread;
#elseif sys
private class Lock {
	public function new() {}

	public inline function release() {}

	public inline function wait(?t:Float) {}
}

private class Mutex {
	public function new() {}

	public inline function acquire() {}

	public inline function release() {}
}

private class Thread {
	public static function create(f:Void->Void) {
		f();
	}
}
#end

/**
	If `haxe.MainLoop` is kept from DCE, then we will insert an `haxe.EntryPoint.run()` call just at then end of `main()`.
	This class can be redefined by custom frameworks so they can handle their own main loop logic.
**/
class EntryPoint {
	#if sys
		static var mutex = new Mutex();
		#if (target.threaded && !cppia)
			static var mainThread:Thread = Thread.current();
		#else
			static var sleepLock = new Lock();
		#end
	#end
	static var pending = new Array<Void->Void>();
	public static var threadCount(default, null):Int = 0;

	/**
		Wakeup a sleeping `run()`
	**/
	public static function wakeup() {
		#if (sys && !(target.threaded && !cppia))
		sleepLock.release();
		#end
	}

	public static function runInMainThread(f:Void->Void) {
		#if sys
			#if (target.threaded && !cppia)
				mainThread.events.run(f);
			#else
				mutex.acquire();
				pending.push(f);
				mutex.release();
				wakeup();
			#end
		#else
		pending.push(f);
		#end
	}

	public static function addThread(f:Void->Void) {
		#if sys
		mutex.acquire();
		threadCount++;
		mutex.release();
		#if (target.threaded && !cppia)
			mainThread.events.promise();
		#end
		Thread.create(function() {
			f();
			mutex.acquire();
			threadCount--;
			if (threadCount == 0)
				wakeup();
			mutex.release();
			#if (target.threaded && !cppia)
				mainThread.events.runPromised(() -> {});
			#end
		});
		#else
		threadCount++;
		pending.push(function() {
			f();
			threadCount--;
		});
		#end
	}

	static function processEvents():Float {
		#if (target.threaded && !cppia)
		return -1;
		#else
		// flush all pending calls
		while (true) {
			#if sys
			mutex.acquire();
			var f = pending.shift();
			mutex.release();
			#else
			var f = pending.shift();
			#end
			if (f == null)
				break;
			f();
		}
		var time = @:privateAccess MainLoop.tick();
		if (!MainLoop.hasEvents() && threadCount == 0)
			return -1;
		return time;
		#end
	}

	/**
		Start the main loop. Depending on the platform, this can return immediately or will only return when the application exits.
	**/
	@:keep public static function run() @:privateAccess {
		#if js
		var nextTick = processEvents();
		inline function setTimeoutNextTick() {
			if (nextTick >= 0) {
				(untyped setTimeout)(run, nextTick * 1000);
			}
		}
		#if nodejs
		setTimeoutNextTick();
		#else
		if(js.Lib.typeof(js.Browser.window) != 'undefined') {
			var window:Dynamic = js.Browser.window;
			var rqf:Dynamic = window.requestAnimationFrame || window.webkitRequestAnimationFrame || window.mozRequestAnimationFrame;
			if(rqf != null) {
				rqf(run);
			} else {
				setTimeoutNextTick();
			}
		} else {
			setTimeoutNextTick();
		}
		#end
		#elseif flash
		flash.Lib.current.stage.addEventListener(flash.events.Event.ENTER_FRAME, function(_) processEvents());
		#elseif (target.threaded && !cppia)
		//everything is delegated to sys.thread.EventLoop
		#elseif lua
		inline function luvRun(mode:String):Bool
			return untyped __lua__('_hx_luv.run({0})', mode);
		while (true) {
			var nextTick = processEvents();
			if(untyped __lua__('_hx_luv.loop_alive()')) {
				if(nextTick < 0)
					luvRun("once")
				else
					luvRun("nowait");
			} else {
				if (nextTick < 0)
					break;
				if (nextTick > 0)
					sleepLock.wait(nextTick);
			}
		}
		#elseif sys
		while (true) {
			var nextTick = processEvents();
			if (nextTick < 0)
				break;
			if (nextTick > 0)
				sleepLock.wait(nextTick); // wait until nextTick or wakeup() call
		}
		#else
		// no implementation available, let's exit immediately
		#end
	}
}
