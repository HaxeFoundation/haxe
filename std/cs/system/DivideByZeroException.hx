package cs.system;

/** The exception that is thrown when there is an attempt to divide an integral or  value by zero. */
@:native("System.DivideByZeroException")
extern class DivideByZeroException extends cs.system.ArithmeticException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
