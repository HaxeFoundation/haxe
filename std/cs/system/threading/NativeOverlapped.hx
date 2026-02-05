package cs.system.threading;

/** Provides an explicit layout that is visible from unmanaged code and that will have the same layout as the Win32 OVERLAPPED structure with additional reserved fields at the end. */
@:native("System.Threading.NativeOverlapped")
extern class NativeOverlapped extends cs.system.ValueType {
	/** Specifies the handle to an event set to the signaled state when the operation is complete. The calling process must set this member either to zero or to a valid event handle before calling any overlapped functions. */
	var EventHandle:cs.system.IntPtr;
	/** Specifies the length of the data transferred. Reserved for operating system use. */
	var InternalHigh:cs.system.IntPtr;
	/** Specifies a system-dependent status. Reserved for operating system use. */
	var InternalLow:cs.system.IntPtr;
	/** Specifies the high word of the byte offset at which to start the transfer. */
	var OffsetHigh:Int;
	/** Specifies a file position at which to start the transfer. */
	var OffsetLow:Int;
}
