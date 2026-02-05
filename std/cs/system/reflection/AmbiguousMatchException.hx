package cs.system.reflection;

/** The exception that is thrown when binding to a member results in more than one member matching the binding criteria. This class cannot be inherited. */
@:native("System.Reflection.AmbiguousMatchException")
extern class AmbiguousMatchException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
