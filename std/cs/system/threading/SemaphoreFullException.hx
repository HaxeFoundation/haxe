package cs.system.threading;

/** The exception that is thrown when the  method is called on a semaphore whose count is already at the maximum. */
@:native("System.Threading.SemaphoreFullException")
extern class SemaphoreFullException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
