package cs.system.threading;

/** Provides a managed representation of a Win32 OVERLAPPED structure, including methods to transfer information from an  instance to a  structure. */
@:native("System.Threading.Overlapped")
extern class Overlapped {
	/**
	 * Gets or sets the object that provides status information on the I/O operation.
	 * @return An object that implements the  interface.
	 */
	var AsyncResult(default, default):cs.system.IAsyncResult;
	/**
	 * Gets or sets the 32-bit integer handle to a synchronization event that is
	 * signaled when the I/O operation is complete.
	 * @return An  value representing the handle of the synchronization event.
	 */
	var EventHandle(default, default):Int;
	/**
	 * Gets or sets the handle to the synchronization event that is signaled when the
	 * I/O operation is complete.
	 * @return An  representing the handle of the event.
	 */
	var EventHandleIntPtr(default, default):cs.system.IntPtr;
	/**
	 * Gets or sets the high-order word of the file position at which to start the
	 * transfer. The file position is a byte offset from the start of the file.
	 * @return An  value representing the high word of the file position.
	 */
	var OffsetHigh(default, default):Int;
	/**
	 * Gets or sets the low-order word of the file position at which to start the
	 * transfer. The file position is a byte offset from the start of the file.
	 * @return An  value representing the low word of the file position.
	 */
	var OffsetLow(default, default):Int;
	@:overload(function():Void {})
	@:overload(function(offsetLo:Int, offsetHi:Int, hEvent:Int, ar:cs.system.IAsyncResult):Void {})
	function new(offsetLo:Int, offsetHi:Int, hEvent:cs.system.IntPtr, ar:cs.system.IAsyncResult):Void;
	/**
	 * Frees the unmanaged memory associated with a native overlapped structure
	 * allocated by the  method.
	 * @param nativeOverlappedPtr A pointer to the  structure to be freed.
	 */
	static function Free(nativeOverlappedPtr:cs.Pointer<cs.system.threading.NativeOverlapped>):Void;
	/**
	 * Unpacks the specified unmanaged  structure into a managed  object.
	 * @param nativeOverlappedPtr An unmanaged pointer to a  structure.
	 * @return An  object containing the information unpacked from the native
	 * structure.
	 */
	static function Unpack(nativeOverlappedPtr:cs.Pointer<cs.system.threading.NativeOverlapped>):cs.system.threading.Overlapped;
	@:overload(function(iocb:cs.system.threading.IOCompletionCallback):cs.Pointer<cs.system.threading.NativeOverlapped> {})
	/**
	 * Packs the current instance into a  structure, specifying the delegate to be
	 * invoked when the asynchronous I/O operation is complete.
	 * @param iocb An  delegate that represents the callback method invoked when the
	 * asynchronous I/O operation completes.
	 * @return An unmanaged pointer to a  structure.
	 */
	function Pack(iocb:cs.system.threading.IOCompletionCallback, userData:Dynamic):cs.Pointer<cs.system.threading.NativeOverlapped>;
	@:overload(function(iocb:cs.system.threading.IOCompletionCallback):cs.Pointer<cs.system.threading.NativeOverlapped> {})
	/**
	 * Packs the current instance into a  structure specifying the delegate to invoke
	 * when the asynchronous I/O operation is complete. Does not propagate the calling
	 * stack.
	 * @param iocb An  delegate that represents the callback method invoked when the
	 * asynchronous I/O operation completes.
	 * @return An unmanaged pointer to a  structure.
	 */
	function UnsafePack(iocb:cs.system.threading.IOCompletionCallback, userData:Dynamic):cs.Pointer<cs.system.threading.NativeOverlapped>;
}
