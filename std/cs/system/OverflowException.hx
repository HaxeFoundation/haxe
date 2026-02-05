package cs.system;

/** The exception that is thrown when an arithmetic, casting, or conversion operation in a checked context results in an overflow. */
@:native("System.OverflowException")
extern class OverflowException extends cs.system.ArithmeticException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
