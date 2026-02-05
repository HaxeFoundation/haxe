package cs.system;

/** The exception that is thrown for errors in an arithmetic, casting, or conversion operation. */
@:native("System.ArithmeticException")
extern class ArithmeticException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
