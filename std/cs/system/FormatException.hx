package cs.system;

/** The exception that is thrown when the format of an argument is invalid, or when a composite format string is not well formed. */
@:native("System.FormatException")
extern class FormatException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
