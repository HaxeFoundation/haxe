package cs.system;

/** The exception that is thrown when an attempt is made to store an element of the wrong type within an array. */
@:native("System.ArrayTypeMismatchException")
extern class ArrayTypeMismatchException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
