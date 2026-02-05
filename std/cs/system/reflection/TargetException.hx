package cs.system.reflection;

/** Represents the exception that is thrown when an attempt is made to invoke an invalid target. */
@:native("System.Reflection.TargetException")
extern class TargetException extends cs.system.ApplicationException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
