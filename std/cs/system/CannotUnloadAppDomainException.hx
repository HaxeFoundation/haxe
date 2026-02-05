package cs.system;

/** The exception that is thrown when an attempt to unload an application domain fails. */
@:native("System.CannotUnloadAppDomainException")
extern class CannotUnloadAppDomainException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
