package cs.system;

/** The exception that is thrown when the execution stack overflows because it contains too many nested method calls. This class cannot be inherited. */
@:native("System.StackOverflowException")
extern class StackOverflowException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
