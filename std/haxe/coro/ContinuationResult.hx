package haxe.coro;

import haxe.Exception;

abstract class ContinuationResult {
	public var _hx_control:ContinuationControl;
	public var _hx_result:Any;
	public var _hx_error:Exception;

	public function toString() {
		return '[ContinuationResult ${_hx_control.toString()}, $_hx_result]';
	}
}