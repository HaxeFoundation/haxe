package js.node;

import haxe.Constraints.Function;
import haxe.extern.Rest;

/**
	The `timer` module exposes a global API for scheduling functions to be called at some future period of time.
	Because the timer functions are globals, there is no need to call `require('timers')` to use the API.

	The timer functions within Node.js implement a similar API as the timers API provided by Web Browsers
	but use a different internal implementation that is built around the Node.js Event Loop.
**/
@:jsRequire("timers")
extern class Timers {
	/**
		Schedules the "immediate" execution of the callback after I/O events' callbacks.

		When multiple calls to `setImmediate()` are made, the `callback` functions are queued for execution
		in the order in which they are created. The entire callback queue is processed every event loop iteration.
		If an immediate timer is queued from inside an executing callback, that timer will not be triggered until
		the next event loop iteration.

		If `callback` is not a function, a `TypeError` will be thrown.

		This method has a custom variant for promises that is available using `util.promisify()`.
	**/
	static function setImmediate(callback:Function, args:Rest<Dynamic>):Immediate;

	/**
		Schedules repeated execution of `callback` every `delay` milliseconds.

		When delay is larger than `2147483647` or less than `1`, the `delay` will be set to `1`.
		Non-integer delays are truncated to an integer.

		If `callback` is not a function, a `TypeError` will be thrown.

		This method has a custom variant for promises that is available using `util.promisify()`.
	**/
	static function setInterval(callback:Function, delay:Int, args:Rest<Dynamic>):Timeout;

	/**
		Schedules execution of a one-time `callback` after `delay` milliseconds.

		The `callback` will likely not be invoked in precisely `delay` milliseconds.
		Node.js makes no guarantees about the exact timing of when callbacks will fire, nor of their ordering.
		The callback will be called as close as possible to the time specified.

		When delay is larger than `2147483647` or less than `1`, the delay will be set to `1`.
		Non-integer delays are truncated to an integer.

		If `callback` is not a function, a `TypeError` will be thrown.

		This method has a custom variant for promises that is available using `util.promisify()`.
	**/
	static function setTimeout(callback:Function, delay:Int, args:Rest<Dynamic>):Timeout;

	/**
		Cancels an Immediate object created by `setImmediate()`.
	**/
	static function clearImmediate(immediate:Immediate):Void;

	/**
		Cancels a Timeout object created by `setInterval()`.
	**/
	static function clearInterval(timeout:Timeout):Void;

	/**
		Cancels a Timeout object created by `setTimeout()`.
	**/
	static function clearTimeout(timeout:Timeout):Void;
}

/**
	This object is created internally and is returned from `setImmediate()`.
	It can be passed to `clearImmediate()` in order to cancel the scheduled actions.
**/
extern class Immediate {
	/**
		If true, the `Immediate` object will keep the Node.js event loop active.
	**/
	function hasRef():Bool;

	/**
		When called, requests that the Node.js event loop not exit so long as the `Immediate` is active.
		Calling `immediate.ref()` multiple times will have no effect.

		By default, all `Immediate` objects are "ref'ed", making it normally unnecessary to call `immediate.ref()`
		unless `immediate.unref()` had been called previously.
	**/
	function ref():Immediate;

	/**
		When called, the active `Immediate` object will not require the Node.js event loop to remain active.
		If there is no other activity keeping the event loop running, the process may exit before the `Immediate` object's
		callback is invoked. Calling immediate.unref() multiple times will have no effect.
	**/
	function unref():Immediate;
}

/**
	This object is created internally and is returned from `setTimeout()` and `setInterval()`.
	It can be passed to either `clearTimeout()` or `clearInterval()` in order to cancel the scheduled actions.
**/
extern class Timeout {
	/**
		If true, the `Timeout` object will keep the Node.js event loop active.
	**/
	function hasRef():Bool;

	/**
		When called, the active `Timeout` object will not require the Node.js event loop to remain active.
		If there is no other activity keeping the event loop running, the process may exit before the `Timeout` object's
		callback is invoked. Calling `timeout.unref()` multiple times will have no effect.

		Calling `timeout.unref()` creates an internal timer that will wake the Node.js event loop.
		Creating too many of these can adversely impact performance of the Node.js application.
	**/
	function ref():Timeout;

	/**
		Sets the timer's start time to the current time, and reschedules the timer to call its callback at the previously
		specified duration adjusted to the current time. This is useful for refreshing a timer without allocating
		a new JavaScript object.

		Using this on a timer that has already called its callback will reactivate the timer.
	**/
	function refresh():Timeout;

	/**
		When called, the active `Timeout` object will not require the Node.js event loop to remain active.
		If there is no other activity keeping the event loop running, the process may exit before the `Timeout` object's
		callback is invoked. Calling `timeout.unref()` multiple times will have no effect.

		Calling `timeout.unref()` creates an internal timer that will wake the Node.js event loop.
		Creating too many of these can adversely impact performance of the Node.js application.
	**/
	function unref():Timeout;
}
