package cs.microsoft.win32.safehandles;

/** Provides a base class for Win32 critical handle implementations in which the value of either 0 or -1 indicates an invalid handle. */
@:native("Microsoft.Win32.SafeHandles.CriticalHandleZeroOrMinusOneIsInvalid")
extern class CriticalHandleZeroOrMinusOneIsInvalid extends cs.system.runtime.interopservices.CriticalHandle {
}
