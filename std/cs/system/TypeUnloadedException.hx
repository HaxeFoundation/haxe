package cs.system;

/** The exception that is thrown when there is an attempt to access an unloaded class. */
@:native("System.TypeUnloadedException")
extern class TypeUnloadedException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
}
