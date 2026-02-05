package cs.system;

/** The exception that is thrown as a wrapper around the exception thrown by the class initializer. This class cannot be inherited. */
@:native("System.TypeInitializationException")
extern class TypeInitializationException extends cs.system.SystemException {
	/**
	 * Gets the fully qualified name of the type that fails to initialize.
	 * @return The fully qualified name of the type that fails to initialize.
	 */
	var TypeName(default, never):String;
	function new(fullTypeName:String, innerException:cs.system.Exception):Void;
	/**
	 * Sets the  object with the type name and additional exception information.
	 * @param info The  that holds the serialized object data about the exception being
	 * thrown.
	 * @param context The  that contains contextual information about the source or
	 * destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
