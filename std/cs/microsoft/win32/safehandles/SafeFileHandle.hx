package cs.microsoft.win32.safehandles;

/** Represents a wrapper class for a file handle. */
@:native("Microsoft.Win32.SafeHandles.SafeFileHandle")
extern class SafeFileHandle extends cs.microsoft.win32.safehandles.SafeHandleZeroOrMinusOneIsInvalid {
	function new(preexistingHandle:cs.system.IntPtr, ownsHandle:Bool):Void;
}
