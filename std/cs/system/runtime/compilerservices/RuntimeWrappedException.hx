package cs.system.runtime.compilerservices;

/** Wraps an exception that does not derive from the  class. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.RuntimeWrappedException")
extern class RuntimeWrappedException extends cs.system.Exception {
	/**
	 * Gets the object that was wrapped by the  object.
	 * @return The object that was wrapped by the  object.
	 */
	var WrappedException(default, never):Dynamic;
	function new(thrownObject:Dynamic):Void;
	/**
	 * Sets the  object with information about the exception.
	 * @param info The  object that holds the serialized object data about the
	 * exception being thrown.
	 * @param context The  object that contains contextual information about the source
	 * or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
