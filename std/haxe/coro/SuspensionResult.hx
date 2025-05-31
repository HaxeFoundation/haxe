package haxe.coro;

import haxe.Exception;

/**
	`SuspensionResult` is the return type of coroutine calls.
**/
abstract class SuspensionResult<T> {
	/**
		The current state of the suspension.
	**/
	public var state:SuspensionState;

	/**
		The result value of the coroutine, if any.
	**/
	public var result:T;

	/**
		The error value of the coroutine, is any.
	**/
	public var error:Exception;

	public function toString() {
		return '[SuspensionResult ${state.toString()}, $result]';
	}
}