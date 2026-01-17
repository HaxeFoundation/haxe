package haxe.coro;

import haxe.Exception;

/**
	Represents a suspension result which immediately has either a result or an error value.
**/
class ImmediateSuspensionResult<T> extends SuspensionResult<T> {
	function new(result:T, error:Exception) {
		super(error == null ? Returned : Thrown);
		this.result  = result;
		this.error   = error;
	}

	/**
		Creates a new `ImmediateSuspensionResult` instance with result `result`.
	**/
	static public function withResult<T>(result:T) {
		return new ImmediateSuspensionResult(result, null);
	}

	/**
		Creates a new `ImmediateSuspensionResult` instance with error `error`.
	**/
	static public function withError<T>(error:T) {
		return new ImmediateSuspensionResult<T>(null, @:privateAccess haxe.Exception.thrown(error));
	}

	public override function toString() {
		return '[ImmediateSuspensionResult ${state.toString()}, $result]';
	}
}