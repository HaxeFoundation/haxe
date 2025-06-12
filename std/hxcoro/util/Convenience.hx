package hxcoro.util;

import haxe.coro.cancellation.ICancellationToken;
import haxe.exceptions.CancellationException;
import haxe.coro.schedulers.Scheduler;
import haxe.Exception;
import haxe.coro.IContinuation;

/**
	A set of convenience functions for working with hxcoro data.
**/
class Convenience {
	/**
		Resumes `cont` with `result` immediately.
	**/
	static public inline function succeedSync<T>(cont:IContinuation<T>, result:T) {
		cont.resume(result, null);
	}

	/**
		Resumes `cont` with exception `error` immediately.
	**/
	static public inline function failSync<T>(cont:IContinuation<T>, error:Exception) {
		cont.resume(null, error);
	}

	/**
		Schedules `cont` to be resumed with `result`.

		Scheduled functions do not increase the call stack and might be executed in a different
		thread if the current dispatcher allows that.
	**/
	static public inline function succeedAsync<T>(cont:IContinuation<T>, result:T) {
		resumeAsync(cont, result, null);
	}

	/**
		Schedules `cont` to be resumed with exception `error`.

		Scheduled functions do not increase the call stack and might be executed in a different
		thread if the current dispatcher allows that.
	**/
	static public inline function failAsync<T>(cont:IContinuation<T>, error:Exception) {
		resumeAsync(cont, null, error);
	}

	/**
		Calls `cont` without any values immediately.
	**/
	static public inline function callSync<T>(cont:IContinuation<T>) {
		cont.resume(null, null);
	}

	/**
		Schedules `cont` to be resumed without any values.

		Scheduled functions do not increase the call stack and might be executed in a different
		thread if the current dispatcher allows that.
	**/
	static public inline function callAsync<T>(cont:IContinuation<T>) {
		resumeAsync(cont, null, null);
	}

	/**
		Schedules `cont` to be resumed with result `result` and exception `error`.

		Scheduled functions do not increase the call stack and might be executed in a different
		thread if the current dispatcher allows that.
	**/
	static public inline function resumeAsync<T>(cont:IContinuation<T>, result:T, error:Exception) {
		cont.context.get(Scheduler).schedule(0, () -> cont.resume(result, error));
	}

	static public inline function orCancellationException(exc:Exception):CancellationException {
		return exc is CancellationException ? cast exc : new CancellationException();
	}

	static public inline function isCancellationRequested(ct:ICancellationToken) {
		return ct.cancellationException != null;
	}
}