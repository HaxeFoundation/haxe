package cs.system.threading;

/** Represents an I/O handle that is bound to the system thread pool and enables low-level components to receive notifications for asynchronous I/O operations. */
@:native("System.Threading.ThreadPoolBoundHandle")
extern class ThreadPoolBoundHandle {
	/**
	 * Gets the bound operating system handle.
	 * @return An object that holds the bound operating system handle.
	 */
	var Handle(default, never):cs.system.runtime.interopservices.SafeHandle;
	/**
	 * Returns a  for the specified handle, which is bound to the system thread pool.
	 * @param handle An object that holds the operating system handle. The handle must
	 * have been opened for overlapped I/O in unmanaged code.
	 * @return A  for , which is bound to the system thread pool.
	 */
	static function BindHandle(handle:cs.system.runtime.interopservices.SafeHandle):cs.system.threading.ThreadPoolBoundHandle;
	/**
	 * Returns the user-provided object that was specified when the  instance was
	 * allocated by calling the  method.
	 * @param overlapped An unmanaged pointer to the  structure from which to return
	 * the associated user-provided object.
	 * @return A user-provided object that distinguishes this  instance from other 
	 * instances, or  if one was not specified when the instance was allocated by
	 * calling the  method.
	 */
	static function GetNativeOverlappedState(overlapped:cs.Pointer<cs.system.threading.NativeOverlapped>):Dynamic;
	@:overload(function(preAllocated:cs.system.threading.PreAllocatedOverlapped):cs.Pointer<cs.system.threading.NativeOverlapped> {})
	/**
	 * Returns an unmanaged pointer to a  structure, specifying a delegate that is
	 * invoked when the asynchronous I/O operation is complete, a user-provided object
	 * that supplies context, and managed objects that serve as buffers.
	 * @param callback A delegate that represents the callback method to invoke when
	 * the asynchronous I/O operation completes.
	 * @param state A user-provided object that distinguishes this  instance from other
	 * instances.
	 * @param pinData An object or array of objects that represent the input or output
	 * buffer for the operation, or . Each object represents a buffer, such an array of
	 * bytes.
	 * @return An unmanaged pointer to a  structure.
	 */
	function AllocateNativeOverlapped(callback:cs.system.threading.IOCompletionCallback, state:Dynamic, pinData:Dynamic):cs.Pointer<cs.system.threading.NativeOverlapped>;
	/** Releases all unmanaged resources used by the  instance. */
	function Dispose():Void;
	/**
	 * Frees the memory associated with a  structure allocated by the  method.
	 * @param overlapped An unmanaged pointer to the  structure to be freed.
	 */
	function FreeNativeOverlapped(overlapped:cs.Pointer<cs.system.threading.NativeOverlapped>):Void;
}
