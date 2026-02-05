package cs.system;

/** The exception that is thrown when a null reference ( in Visual Basic) is passed to a method that does not accept it as a valid argument. */
@:native("System.ArgumentNullException")
extern class ArgumentNullException extends cs.system.ArgumentException {
	@:overload(function():Void {})
	@:overload(function(paramName:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(paramName:String, message:String):Void;
}
