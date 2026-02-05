package cs.system.threading;

/** Provides convenience methods to for working with a safe handle for a wait handle. */
@:native("System.Threading.WaitHandleExtensions")
extern class WaitHandleExtensions {
	/**
	 * Gets the safe handle for a native operating system wait handle.
	 * @param waitHandle A native operating system handle.
	 * @return The safe wait handle that wraps the native operating system wait handle.
	 */
	static function GetSafeWaitHandle(waitHandle:cs.system.threading.WaitHandle):cs.microsoft.win32.safehandles.SafeWaitHandle;
	/**
	 * Sets a safe handle for a native operating system wait handle.
	 * @param waitHandle A wait handle that encapsulates an operating system-specific
	 * object that waits for exclusive access to a shared resource.
	 * @param value The safe handle to wrap the operating system handle.
	 */
	static function SetSafeWaitHandle(waitHandle:cs.system.threading.WaitHandle, value:cs.microsoft.win32.safehandles.SafeWaitHandle):Void;
}
