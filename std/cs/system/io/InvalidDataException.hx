package cs.system.io;

/** The exception that is thrown when a data stream is in an invalid format. */
@:native("System.IO.InvalidDataException")
extern class InvalidDataException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
