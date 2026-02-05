package cs.system;

/** The exception that is thrown when an attempt is made to access an unloaded application domain. */
@:native("System.AppDomainUnloadedException")
extern class AppDomainUnloadedException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
