package cs.system.diagnostics;

/** Provides information about a , which represents a function call on the call stack for the current thread. */
@:native("System.Diagnostics.StackFrame")
extern class StackFrame {
	/** Defines the value that is returned from the  or  method when the native or Microsoft intermediate language (MSIL) offset is unknown. This field is constant. */
	static var OFFSET_UNKNOWN(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(fNeedFileInfo:Bool):Void {})
	@:overload(function(skipFrames:Int):Void {})
	@:overload(function(skipFrames:Int, fNeedFileInfo:Bool):Void {})
	@:overload(function(fileName:String, lineNumber:Int):Void {})
	function new(fileName:String, lineNumber:Int, colNumber:Int):Void;
	/**
	 * Gets the column number in the file that contains the code that is executing.
	 * This information is typically extracted from the debugging symbols for the
	 * executable.
	 * @return The file column number, or 0 (zero) if the file column number cannot be
	 * determined.
	 */
	function GetFileColumnNumber():Int;
	/**
	 * Gets the line number in the file that contains the code that is executing. This
	 * information is typically extracted from the debugging symbols for the
	 * executable.
	 * @return The file line number, or 0 (zero) if the file line number cannot be
	 * determined.
	 */
	function GetFileLineNumber():Int;
	/**
	 * Gets the file name that contains the code that is executing. This information is
	 * typically extracted from the debugging symbols for the executable.
	 * @return The file name, or  if the file name cannot be determined.
	 */
	function GetFileName():String;
	/**
	 * Gets the offset from the start of the Microsoft intermediate language (MSIL)
	 * code for the method that is executing. This offset might be an approximation
	 * depending on whether or not the just-in-time (JIT) compiler is generating
	 * debugging code. The generation of this debugging information is controlled by
	 * the .
	 * @return The offset from the start of the MSIL code for the method that is
	 * executing.
	 */
	function GetILOffset():Int;
	/**
	 * Gets the method in which the frame is executing.
	 * @return The method in which the frame is executing.
	 */
	function GetMethod():cs.system.reflection.MethodBase;
	/**
	 * Gets the offset from the start of the native just-in-time (JIT)-compiled code
	 * for the method that is being executed. The generation of this debugging
	 * information is controlled by the  class.
	 * @return The offset from the start of the JIT-compiled code for the method that
	 * is being executed.
	 */
	function GetNativeOffset():Int;
	/**
	 * Builds a readable representation of the stack trace.
	 * @return A readable representation of the stack trace.
	 */
	function ToString():String;
}
