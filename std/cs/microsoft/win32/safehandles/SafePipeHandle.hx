package cs.microsoft.win32.safehandles;

/** Represents a wrapper class for a pipe handle. */
@:native("Microsoft.Win32.SafeHandles.SafePipeHandle")
extern class SafePipeHandle extends cs.microsoft.win32.safehandles.SafeHandleZeroOrMinusOneIsInvalid {
	function new(preexistingHandle:cs.system.IntPtr, ownsHandle:Bool):Void;
}
