package cs.system;

/** The exception that is thrown when there is insufficient execution stack available to allow most methods to execute. */
@:native("System.InsufficientExecutionStackException")
extern class InsufficientExecutionStackException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
