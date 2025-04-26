package haxe.coro;

import haxe.Exception;

abstract class SuspensionResult<T> {
	public var control:SuspensionState;
	public var result:T;
	public var error:Exception;

	public function toString() {
		return '[SuspensionResult ${control.toString()}, $result]';
	}
}