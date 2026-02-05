package cs.system.reflection;

/** The exception that is thrown by methods invoked through reflection. This class cannot be inherited. */
@:native("System.Reflection.TargetInvocationException")
extern class TargetInvocationException extends cs.system.ApplicationException {
	@:overload(function(inner:cs.system.Exception):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
