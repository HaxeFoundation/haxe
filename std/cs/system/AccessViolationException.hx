package cs.system;

/** The exception that is thrown when there is an attempt to read or write protected memory. */
@:native("System.AccessViolationException")
extern class AccessViolationException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
