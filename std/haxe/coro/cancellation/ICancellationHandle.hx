package haxe.coro.cancellation;

/**
 * Handle to a callback which has been registered with a cancellation token.
 */
interface ICancellationHandle {
	/**
	 * Stops the callback from being executed when the token is cancelled.
	 * If the token is already cancelled this function does nothing.
	 * This function is safe to call multiple times and is thread safe.
	 */
	function close():Void;
}
