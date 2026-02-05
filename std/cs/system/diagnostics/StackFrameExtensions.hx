package cs.system.diagnostics;

/** Provides extension methods for the  class, which represents a function call on the call stack for the current thread. */
@:native("System.Diagnostics.StackFrameExtensions")
extern class StackFrameExtensions {
	/**
	 * Returns a pointer to the base address of the native image that this stack frame
	 * is executing.
	 * @param stackFrame A stack frame.
	 * @return A pointer to the base address of the native image or  if you're
	 * targeting the .NET Framework.
	 */
	static function GetNativeImageBase(stackFrame:cs.system.diagnostics.StackFrame):cs.system.IntPtr;
	/**
	 * Gets an interface pointer to the start of the native code for the method that is
	 * being executed.
	 * @param stackFrame A stack frame.
	 * @return An interface pointer to the start of the native code for the method that
	 * is being executed or  if you're targeting the .NET Framework.
	 */
	static function GetNativeIP(stackFrame:cs.system.diagnostics.StackFrame):cs.system.IntPtr;
	/**
	 * Indicates whether an offset from the start of the IL code for the method that is
	 * executing is available.
	 * @param stackFrame A stack frame.
	 * @return if the offset is available; otherwise, .
	 */
	static function HasILOffset(stackFrame:cs.system.diagnostics.StackFrame):Bool;
	/**
	 * Indicates whether information about the method in which the specified frame is
	 * executing is available.
	 * @param stackFrame A stack frame.
	 * @return if information about the method in which the current frame is executing
	 * is available; otherwise, .
	 */
	static function HasMethod(stackFrame:cs.system.diagnostics.StackFrame):Bool;
	/**
	 * Indicates whether the native image is available for the specified stack frame.
	 * @param stackFrame A stack frame.
	 * @return if a native image is available for this stack frame; otherwise, .
	 */
	static function HasNativeImage(stackFrame:cs.system.diagnostics.StackFrame):Bool;
	/**
	 * Indicates whether the file that contains the code that the specified stack frame
	 * is executing is available.
	 * @param stackFrame A stack frame.
	 * @return if the code that the specified stack frame is executing is available;
	 * otherwise, .
	 */
	static function HasSource(stackFrame:cs.system.diagnostics.StackFrame):Bool;
}
