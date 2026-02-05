package cs.system;

/** The exception that is thrown when there is an attempt to combine two delegates based on the  type instead of the  type. This class cannot be inherited. */
@:native("System.MulticastNotSupportedException")
extern class MulticastNotSupportedException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
