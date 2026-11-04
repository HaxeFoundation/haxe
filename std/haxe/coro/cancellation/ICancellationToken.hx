package haxe.coro.cancellation;

import haxe.exceptions.CancellationException;

/**
 * A cancellation token enables cooperative cancellation between units of work (threads, coroutines, etc).
 * The token cannot be used to initiate cancellation, only to poll for a cancellation request or register a callback for when cancellation is requested.
 * Cancellation is cooperative which means it is up to the caller to respond to a cancellation request in a manner it deems best.
 * Access to this interface is thread safe.
 */
interface ICancellationToken {
	var cancellationException (get, never) : Null<CancellationException>;

	/**
	 * Register a callback which will be executed when this token is cancelled.
	 *
	 * If this token has already been cancelled the function will be executed immediately and synchronously, any exception raised will not be caught.
	 * The thread the callback is executed on is implementation defined if cancellation has not been yet been requested.
	 * @param func Callback to be executed when the token is cancelled.
	 * @return Cancellation handle which can be used to cancel the callback from executing.
	 */
	function onCancellationRequested(handle : ICancellationCallback) : ICancellationHandle;
}
