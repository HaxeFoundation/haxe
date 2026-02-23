package haxe.coro;

import haxe.Exception;

/**
	`SuspensionResult` is the return type of coroutine calls.
**/
class SuspensionResult<T> {

	static public final suspended = new SuspensionResult<Any>(Pending);

	/**
		Creates a new instance with the given `state`.
	**/
	public function new(state:SuspensionState) {
		this.state = state;
	}

	/**
		The current state of the suspension.
	**/
	public var state:SuspensionState;

	/**
		The result value of the coroutine, if any.
	**/
	public var result:Null<T>;

	/**
		The error value of the coroutine, is any.
	**/
	public var error:Null<Exception>;

	public function toString() {
		return '[SuspensionResult ${state.toString()}, $result]';
	}


	/**
		Creates a new `SuspensionResult` instance with result `result`.
	**/
	static public function withResult<T>(result:T) {
		final res = new SuspensionResult(Returned);
		res.result = result;
		return res;
	}

	/**
		Creates a new `SuspensionResult` instance with error `error`.
	**/
	static public function withError<T>(error:Exception) {
		final res = new SuspensionResult(Thrown);
		res.error = error;
		return res;
	}
}