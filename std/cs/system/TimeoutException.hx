package cs.system;

/** The exception that is thrown when the time allotted for a process or operation has expired. */
@:native("System.TimeoutException")
extern class TimeoutException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
