package haxe.coro;

import haxe.Exception;

class ImmediateContinuationResult<T> extends ContinuationResult<T> {
	function new(result:T, error:Exception) {
		_hx_result = result;
		_hx_error = error;
		_hx_control = error == null ? Returned : Thrown;
	}

	static public function withResult<T>(result:T) {
		return new ImmediateContinuationResult(result, null);
	}

	static public function withError<T>(error:T) {
		return new ImmediateContinuationResult<T>(null, @:privateAccess haxe.Exception.thrown(error));
	}

	public override function toString() {
		return '[ImmediateContinuationResult ${_hx_control.toString()}, $_hx_result]';
	}
}