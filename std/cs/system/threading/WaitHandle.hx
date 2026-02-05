package cs.system.threading;

/** Encapsulates operating system-specific objects that wait for exclusive access to shared resources. */
@:native("System.Threading.WaitHandle")
extern class WaitHandle extends cs.system.MarshalByRefObject {
	/** Indicates that a  operation timed out before any of the wait handles were signaled. This field is constant. */
	static var WaitTimeout(default, never):Int;
	/**
	 * Gets or sets the native operating system handle.
	 * @return An  representing the native operating system handle. The default is the
	 * value of the  field.
	 */
	var Handle(default, default):cs.system.IntPtr;
	/**
	 * Gets or sets the native operating system handle.
	 * @return A  representing the native operating system handle.
	 */
	var SafeWaitHandle(default, default):cs.microsoft.win32.safehandles.SafeWaitHandle;
	@:overload(function(toSignal:cs.system.threading.WaitHandle, toWaitOn:cs.system.threading.WaitHandle):Bool {})
	@:overload(function(toSignal:cs.system.threading.WaitHandle, toWaitOn:cs.system.threading.WaitHandle, millisecondsTimeout:Int, exitContext:Bool):Bool {})
	/**
	 * Signals one  and waits on another.
	 * @param toSignal The  to signal.
	 * @param toWaitOn The  to wait on.
	 * @return if both the signal and the wait complete successfully; if the wait does
	 * not complete, the method does not return.
	 */
	static function SignalAndWait(toSignal:cs.system.threading.WaitHandle, toWaitOn:cs.system.threading.WaitHandle, timeout:cs.system.TimeSpan, exitContext:Bool):Bool;
	@:overload(function(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>):Bool {})
	@:overload(function(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>, millisecondsTimeout:Int):Bool {})
	@:overload(function(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>, timeout:cs.system.TimeSpan):Bool {})
	@:overload(function(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>, millisecondsTimeout:Int, exitContext:Bool):Bool {})
	/**
	 * Waits for all the elements in the specified array to receive a signal.
	 * @param waitHandles A  array containing the objects for which the current
	 * instance will wait. This array cannot contain multiple references to the same
	 * object.
	 * @return when every element in  has received a signal; otherwise the method never
	 * returns.
	 */
	static function WaitAll(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>, timeout:cs.system.TimeSpan, exitContext:Bool):Bool;
	@:overload(function(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>):Int {})
	@:overload(function(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>, millisecondsTimeout:Int):Int {})
	@:overload(function(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>, timeout:cs.system.TimeSpan):Int {})
	@:overload(function(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>, millisecondsTimeout:Int, exitContext:Bool):Int {})
	/**
	 * Waits for any of the elements in the specified array to receive a signal.
	 * @param waitHandles A  array containing the objects for which the current
	 * instance will wait.
	 * @return The array index of the object that satisfied the wait.
	 */
	static function WaitAny(waitHandles:cs.NativeArray<cs.system.threading.WaitHandle>, timeout:cs.system.TimeSpan, exitContext:Bool):Int;
	/** Releases all resources held by the current . */
	function Close():Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function():Bool {})
	@:overload(function(millisecondsTimeout:Int):Bool {})
	@:overload(function(timeout:cs.system.TimeSpan):Bool {})
	@:overload(function(millisecondsTimeout:Int, exitContext:Bool):Bool {})
	/**
	 * Blocks the current thread until the current  receives a signal.
	 * @return if the current instance receives a signal. If the current instance is
	 * never signaled,  never returns.
	 */
	function WaitOne(timeout:cs.system.TimeSpan, exitContext:Bool):Bool;
}
