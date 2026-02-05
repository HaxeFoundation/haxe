package cs.system.runtime.interopservices;

/** The exception thrown when the type of the incoming  does not match the type specified in the managed signature. */
@:native("System.Runtime.InteropServices.SafeArrayTypeMismatchException")
extern class SafeArrayTypeMismatchException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
