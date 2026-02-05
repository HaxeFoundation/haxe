package cs.system;

/** The exception that is thrown when an object appears more than once in an array of synchronization objects. */
@:native("System.DuplicateWaitObjectException")
extern class DuplicateWaitObjectException extends cs.system.ArgumentException {
	@:overload(function():Void {})
	@:overload(function(parameterName:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(parameterName:String, message:String):Void;
}
