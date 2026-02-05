package cs.system.threading;

/**
 * Receives the error code, number of bytes, and overlapped value type when an I/O
 * operation completes on the thread pool.
 * @param errorCode The error code.
 * @param numBytes The number of bytes that are transferred.
 * @param pOVERLAP A  representing an unmanaged pointer to the native overlapped
 * value type.
 */
@:native("System.Threading.IOCompletionCallback")
extern class IOCompletionCallback extends cs.system.MulticastDelegate {
	function new(func:(errorCode:cs.UInt, numBytes:cs.UInt, pOVERLAP:cs.Pointer<cs.system.threading.NativeOverlapped>)->Void):Void;
	function Invoke(errorCode:cs.UInt, numBytes:cs.UInt, pOVERLAP:cs.Pointer<cs.system.threading.NativeOverlapped>):Void;
}
