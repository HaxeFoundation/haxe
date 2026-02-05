package cs.system;

/** The exception that is thrown when an operation is performed on a disposed object. */
@:native("System.ObjectDisposedException")
extern class ObjectDisposedException extends cs.system.InvalidOperationException {
	/**
	 * Gets the name of the disposed object.
	 * @return A string containing the name of the disposed object.
	 */
	var ObjectName(default, never):String;
	@:overload(function(objectName:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(objectName:String, message:String):Void;
	/**
	 * Retrieves the  object with the parameter name and additional exception
	 * information.
	 * @param info The  that holds the serialized object data about the exception being
	 * thrown.
	 * @param context The  that contains contextual information about the source or
	 * destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
