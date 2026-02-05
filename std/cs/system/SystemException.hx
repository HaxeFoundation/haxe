package cs.system;

/** Serves as the base class for system exceptions namespace. */
@:native("System.SystemException")
extern class SystemException extends cs.system.Exception {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
