package cs.system.runtime.interopservices;

/** The exception thrown when the rank of an incoming  does not match the rank specified in the managed signature. */
@:native("System.Runtime.InteropServices.SafeArrayRankMismatchException")
extern class SafeArrayRankMismatchException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
