package cs.system.diagnostics;

/** Represents a stack trace, which is an ordered collection of one or more stack frames. */
@:native("System.Diagnostics.StackTrace")
extern class StackTrace {
	/** Defines the default for the number of methods to omit from the stack trace. This field is constant. */
	static var METHODS_TO_SKIP(default, never):Int;
	/**
	 * Gets the number of frames in the stack trace.
	 * @return The number of frames in the stack trace.
	 */
	var FrameCount(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(fNeedFileInfo:Bool):Void {})
	@:overload(function(frame:cs.system.diagnostics.StackFrame):Void {})
	@:overload(function(e:cs.system.Exception):Void {})
	@:overload(function(skipFrames:Int):Void {})
	@:overload(function(e:cs.system.Exception, fNeedFileInfo:Bool):Void {})
	@:overload(function(e:cs.system.Exception, skipFrames:Int):Void {})
	@:overload(function(skipFrames:Int, fNeedFileInfo:Bool):Void {})
	function new(e:cs.system.Exception, skipFrames:Int, fNeedFileInfo:Bool):Void;
	/**
	 * Gets the specified stack frame.
	 * @param index The index of the stack frame requested.
	 * @return The specified stack frame.
	 */
	function GetFrame(index:Int):cs.system.diagnostics.StackFrame;
	/**
	 * Returns a copy of all stack frames in the current stack trace.
	 * @return An array of type  representing the function calls in the stack trace.
	 */
	function GetFrames():cs.NativeArray<cs.system.diagnostics.StackFrame>;
	/**
	 * Builds a readable representation of the stack trace.
	 * @return A readable representation of the stack trace.
	 */
	function ToString():String;
}
