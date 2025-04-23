package haxe.coro;

import haxe.Exception;

abstract class SuspensionResult<T> {
	public var _hx_control:SuspensionState;
	public var _hx_result:T;
	public var _hx_error:Exception;

	public function toString() {
		return '[SuspensionResult ${_hx_control.toString()}, $_hx_result]';
	}
}