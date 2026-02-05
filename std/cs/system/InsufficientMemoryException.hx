package cs.system;

/** The exception that is thrown when a check for sufficient available memory fails. This class cannot be inherited. */
@:native("System.InsufficientMemoryException")
extern class InsufficientMemoryException extends cs.system.OutOfMemoryException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
