package cs.system.reflection;

/** The exception that is thrown when the number of parameters for an invocation does not match the number expected. This class cannot be inherited. */
@:native("System.Reflection.TargetParameterCountException")
extern class TargetParameterCountException extends cs.system.ApplicationException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
