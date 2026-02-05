package cs.microsoft.win32.safehandles;

/** Provides a managed wrapper for a process handle. */
@:native("Microsoft.Win32.SafeHandles.SafeProcessHandle")
extern class SafeProcessHandle extends cs.microsoft.win32.safehandles.SafeHandleZeroOrMinusOneIsInvalid {
	function new(existingHandle:cs.system.IntPtr, ownsHandle:Bool):Void;
}
