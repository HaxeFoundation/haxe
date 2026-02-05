package cs.microsoft.win32.safehandles;

/** Represents a wrapper class for a wait handle. */
@:native("Microsoft.Win32.SafeHandles.SafeWaitHandle")
extern class SafeWaitHandle extends cs.microsoft.win32.safehandles.SafeHandleZeroOrMinusOneIsInvalid {
	function new(existingHandle:cs.system.IntPtr, ownsHandle:Bool):Void;
}
