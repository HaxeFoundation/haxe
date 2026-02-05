package cs.system.io;

/** The exception that is thrown when an attempt to access a file that does not exist on disk fails. */
@:native("System.IO.FileNotFoundException")
extern class FileNotFoundException extends cs.system.io.IOException {
	/**
	 * Gets the name of the file that cannot be found.
	 * @return The name of the file, or  if no file name was passed to the constructor
	 * for this instance.
	 */
	var FileName(default, never):String;
	/**
	 * Gets the log file that describes why loading of an assembly failed.
	 * @return The errors reported by the assembly cache.
	 */
	var FusionLog(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(message:String, fileName:String):Void {})
	function new(message:String, fileName:String, innerException:cs.system.Exception):Void;
	/**
	 * Sets the  object with the file name and additional exception information.
	 * @param info The object that holds the serialized object data about the exception
	 * being thrown.
	 * @param context The object that contains contextual information about the source
	 * or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Returns the fully qualified name of this exception and possibly the error
	 * message, the name of the inner exception, and the stack trace.
	 * @return The fully qualified name of this exception and possibly the error
	 * message, the name of the inner exception, and the stack trace.
	 */
	function ToString():String;
}
