package cs.system;

/** Provides a mechanism for releasing unmanaged resources asynchronously. */
@:native("System.IAsyncDisposable")
extern interface IAsyncDisposable {
	/**
	 * Performs application-defined tasks associated with freeing, releasing, or
	 * resetting unmanaged resources asynchronously.
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
}
