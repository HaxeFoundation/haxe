package haxe.coro;

import haxe.Exception;
import haxe.coro.context.Context;

/**
	This interface represents an object which can be resumed via its `resume` function.
**/
interface IContinuation<T> {
	/**
		The immutable context of this object.
	**/
	var context(get, never):Context;

	/**
		Resumes execution with result value `result` or exception `error`.

		Generally, if `error != null`, the result value is ignoried and execution
		continues as if `error` had been thrown as an exception.
	**/
	function resume(result:Null<T>, error:Null<Exception>):Void;
}
