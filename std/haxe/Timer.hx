/*
 * Copyright (C)2005-2017 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package haxe;

/**
	The Timer class allows you to create asynchronous timers on platforms that
	support events.

	The intended usage is to create an instance of the Timer class with a given
	interval, set its run() method to a custom function to be invoked and
	eventually call stop() to stop the Timer.

	Note that a running Timer may or may not prevent the program to exit
	automatically when main() returns.

	It is also possible to extend this class and override its run() method in
	the child class.
**/
class Timer {

	#if (flash || js)
		private var id : Null<Int>;
	#elseif java
		private var timer : java.util.Timer;
		private var task : java.util.TimerTask;
	#else
		private var event : MainLoop.MainEvent;
	#end

	/**
		Creates a new timer that will run every `time_ms` milliseconds.

		After creating the Timer instance, it calls `this.run` repeatedly,
		with delays of `time_ms` milliseconds, until `this.stop` is called.

		The first invocation occurs after `time_ms` milliseconds, not
		immediately.

		The accuracy of this may be platform-dependent.
	**/
	public function new( time_ms : Int ){
		#if flash
			var me = this;
			id = untyped __global__["flash.utils.setInterval"](function() { me.run(); },time_ms);
		#elseif js
			var me = this;
			id = untyped setInterval(function() me.run(),time_ms);
		#elseif java
			timer = new java.util.Timer();
			timer.scheduleAtFixedRate(task = new TimerTask(this), haxe.Int64.ofInt(time_ms), haxe.Int64.ofInt(time_ms));
		#else
			var dt = time_ms / 1000;
			event = MainLoop.add(function() {
				@:privateAccess event.nextRun += dt;
				run();
			});
			event.delay(dt);
		#end
	}

	/**
		Stops `this` Timer.

		After calling this method, no additional invocations of `this.run`
		will occur.

		It is not possible to restart `this` Timer once stopped.
	**/
	public function stop() {
		#if (flash || js)
			if( id == null )
				return;
			#if flash
				untyped __global__["flash.utils.clearInterval"](id);
			#elseif js
				untyped clearInterval(id);
			#end
			id = null;
		#elseif java
			if(timer != null) {
				timer.cancel();
				timer = null;
			}
			task = null;
		#else
			if( event != null ) {
				event.stop();
				event = null;
			}
		#end
	}

	/**
		This method is invoked repeatedly on `this` Timer.

		It can be overridden in a subclass, or rebound directly to a custom
		function:
			var timer = new haxe.Timer(1000); // 1000ms delay
			timer.run = function() { ... }

		Once bound, it can still be rebound to different functions until `this`
		Timer is stopped through a call to `this.stop`.
	**/
	public dynamic function run() {

	}

	/**
		Invokes `f` after `time_ms` milliseconds.

		This is a convenience function for creating a new Timer instance with
		`time_ms` as argument, binding its run() method to `f` and then stopping
		`this` Timer upon the first invocation.

		If `f` is null, the result is unspecified.
	**/
	public static function delay( f : Void -> Void, time_ms : Int ) {
		var t = new haxe.Timer(time_ms);
		t.run = function() {
			t.stop();
			f();
		};
		return t;
	}

	/**
		Measures the time it takes to execute `f`, in seconds with fractions.

		This is a convenience function for calculating the difference between
		Timer.stamp() before and after the invocation of `f`.

		The difference is passed as argument to Log.trace(), with "s" appended
		to denote the unit. The optional `pos` argument is passed through.

		If `f` is null, the result is unspecified.
	**/
	public static function measure<T>( f : Void -> T, ?pos : PosInfos ) : T {
		var t0 = stamp();
		var r = f();
		Log.trace((stamp() - t0) + "s", pos);
		return r;
	}

	/**
		Returns a timestamp, in seconds with fractions.

		The value itself might differ depending on platforms, only differences
		between two values make sense.
	**/
	public static inline function stamp() : Float {
		#if flash
			return flash.Lib.getTimer() / 1000;
		#elseif (neko || php)
			return Sys.time();
		#elseif js
			return Date.now().getTime() / 1000;
		#elseif cpp
			return untyped __global__.__time_stamp();
		#elseif python
			return Sys.cpuTime();
		#elseif sys
			return Sys.time();

		#else
			return 0;
		#end
	}

}

#if java
@:nativeGen
private class TimerTask extends java.util.TimerTask {
	var timer:Timer;
	public function new(timer:Timer):Void {
		super();
		this.timer = timer;
	}

	@:overload override public function run():Void {
		timer.run();
	}
}
#end
