package cs.system;

/** The exception that is thrown when there is not enough memory to continue the execution of a program. */
@:native("System.OutOfMemoryException")
extern class OutOfMemoryException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
