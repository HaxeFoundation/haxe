package cs.system.runtime.serialization;

/** The exception thrown when an error occurs during serialization or deserialization. */
@:native("System.Runtime.Serialization.SerializationException")
extern class SerializationException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
