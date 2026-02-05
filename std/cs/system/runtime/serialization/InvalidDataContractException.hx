package cs.system.runtime.serialization;

/** The exception that is thrown when the  or  encounters an invalid data contract during serialization and deserialization. */
@:native("System.Runtime.Serialization.InvalidDataContractException")
extern class InvalidDataContractException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
