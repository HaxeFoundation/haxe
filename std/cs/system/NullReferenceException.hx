package cs.system;

/** The exception that is thrown when there is an attempt to dereference a null object reference. */
@:native("System.NullReferenceException")
extern class NullReferenceException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
