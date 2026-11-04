package haxe.coro;

/**
	The state of a coroutine.
**/
@:using(SuspensionState.SuspensionStateTools)
enum abstract SuspensionState(Int) {
	/**
		The coroutine is still running.
	**/
	final Pending;

	/**
		The coroutine has returned a value.
	**/
	final Returned;

	/**
		The coroutine has thrown an exception.
	**/
	final Thrown;
}

class SuspensionStateTools {
	static public function toString(c:SuspensionState) {
		return switch (c) {
			case Pending: "Pending";
			case Returned: "Returned";
			case Thrown: "Thrown";
		}
	}
}