package cs.system;

/** Represents the status of an asynchronous operation. */
@:native("System.IAsyncResult")
extern interface IAsyncResult {
	/**
	 * Gets a user-defined object that qualifies or contains information about an
	 * asynchronous operation.
	 * @return A user-defined object that qualifies or contains information about an
	 * asynchronous operation.
	 */
	var AsyncState(default, never):Dynamic;
	/**
	 * Gets a  that is used to wait for an asynchronous operation to complete.
	 * @return A  that is used to wait for an asynchronous operation to complete.
	 */
	var AsyncWaitHandle(default, never):cs.system.threading.WaitHandle;
	/**
	 * Gets a value that indicates whether the asynchronous operation completed
	 * synchronously.
	 * @return if the asynchronous operation completed synchronously; otherwise, .
	 */
	var CompletedSynchronously(default, never):Bool;
	/**
	 * Gets a value that indicates whether the asynchronous operation has completed.
	 * @return if the operation is complete; otherwise, .
	 */
	var IsCompleted(default, never):Bool;
}
