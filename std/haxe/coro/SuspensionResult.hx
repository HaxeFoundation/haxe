package haxe.coro;

import haxe.Exception;

abstract class SuspensionResult<T> {
	public var state:SuspensionState;
	public var result:T;
	public var error:Exception;

	public function toString() {
		return '[SuspensionResult ${state.toString()}, $result]';
	}
}