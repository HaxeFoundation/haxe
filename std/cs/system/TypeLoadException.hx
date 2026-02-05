package cs.system;

/** The exception that is thrown when type-loading failures occur. */
@:native("System.TypeLoadException")
extern class TypeLoadException extends cs.system.SystemException {
	/**
	 * Gets the fully qualified name of the type that causes the exception.
	 * @return The fully qualified type name.
	 */
	var TypeName(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
	/**
	 * Sets the  object with the class name, method name, resource ID, and additional
	 * exception information.
	 * @param info The object that holds the serialized object data.
	 * @param context The contextual information about the source or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
