package cs.system;

/** The exception that is thrown when the operating system denies access because of an I/O error or a specific type of security error. */
@:native("System.UnauthorizedAccessException")
extern class UnauthorizedAccessException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
