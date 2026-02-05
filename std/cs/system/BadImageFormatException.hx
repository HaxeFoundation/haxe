package cs.system;

/** The exception that is thrown when the file image of a dynamic link library (DLL) or an executable program is invalid. */
@:native("System.BadImageFormatException")
extern class BadImageFormatException extends cs.system.SystemException {
	/**
	 * Gets the name of the file that causes this exception.
	 * @return The name of the file with the invalid image, or a null reference if no
	 * file name was passed to the constructor for the current instance.
	 */
	var FileName(default, never):String;
	/**
	 * Gets the log file that describes why an assembly load failed.
	 * @return A  containing errors reported by the assembly cache.
	 */
	var FusionLog(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	@:overload(function(message:String, fileName:String):Void {})
	function new(message:String, fileName:String, inner:cs.system.Exception):Void;
	/**
	 * Sets the  object with the file name, assembly cache log, and additional
	 * exception information.
	 * @param info The  that holds the serialized object data about the exception being
	 * thrown.
	 * @param context The  that contains contextual information about the source or
	 * destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Returns the fully qualified name of this exception and possibly the error
	 * message, the name of the inner exception, and the stack trace.
	 * @return A string containing the fully qualified name of this exception and
	 * possibly the error message, the name of the inner exception, and the stack
	 * trace.
	 */
	function ToString():String;
}
