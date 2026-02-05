package cs.system;

/** The exception that is thrown when a method call is invalid for the object's current state. */
@:native("System.InvalidOperationException")
extern class InvalidOperationException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
