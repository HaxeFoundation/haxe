package cs.system;

/** The exception that is thrown when a method attempts to use a type that it does not have access to. */
@:native("System.TypeAccessException")
extern class TypeAccessException extends cs.system.TypeLoadException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
