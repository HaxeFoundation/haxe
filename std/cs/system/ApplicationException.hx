package cs.system;

/** Serves as the base class for application-defined exceptions. */
@:native("System.ApplicationException")
extern class ApplicationException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
