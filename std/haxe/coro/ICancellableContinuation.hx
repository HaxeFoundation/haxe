package haxe.coro;

import haxe.exceptions.CancellationException;

/**
 * Cancellable continuations are continuations which supports asynchronous cancellation.
 * Like standard continuations they can be explicitly resumed by the user, but unlike standard
 * continuations they will be automatically resumed with a `haxe.exceptions.CancellationException` when cancelled.
 *
 * This interface provides a callback property which if set will be invoked when cancellation is requested
 * allowing you to cleanup resources. The callback will only be invoked on cancellation, not resuming with a result or exception.
 */
interface ICancellableContinuation<T> extends IContinuation<T> {
	/**
	 * Register a function to be invoked upon cancellation of the continuation.
	 * If the continuation is already cancelled the function will be invoked immediately.
	 * Attempting to set this property multiple times will result in an exception being raised.
	 */
	var onCancellationRequested (never, set) : CancellationException->Void;
}