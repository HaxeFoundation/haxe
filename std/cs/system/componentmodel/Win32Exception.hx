package cs.system.componentmodel;

/** Throws an exception for a Win32 error code. */
@:native("System.ComponentModel.Win32Exception")
extern class Win32Exception extends cs.system.runtime.interopservices.ExternalException {
	/**
	 * Gets the Win32 error code associated with this exception.
	 * @return The Win32 error code associated with this exception.
	 */
	var NativeErrorCode(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(error:Int):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(error:Int, message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
	/**
	 * Sets the  object with the file name and line number at which this  occurred.
	 * @param info A .
	 * @param context The contextual information about the source or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	function ToString():String;
}
