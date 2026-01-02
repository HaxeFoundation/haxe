package haxe.coro.cancellation;

import haxe.exceptions.CancellationException;
import haxe.coro.context.Key;

private class NoOpCancellationHandle implements ICancellationHandle {
	public function new() {}
	public function close() {}
}

private class NoOpCancellationToken implements ICancellationToken {
	static final handle = new NoOpCancellationHandle();

	public var cancellationException (get, never) : Null<CancellationException>;

	public function new() {}

	public function onCancellationRequested(_:ICancellationCallback):ICancellationHandle {
		return handle;
	}
	public function get_cancellationException() {
		return null;
	}
}

class CancellationToken {
	public static final key = new Key<ICancellationToken>('CancellationToken');

	/**
	 * Returns a cancellation token which will never be cancelled.
	 */
	public static final none : ICancellationToken = new NoOpCancellationToken();
}