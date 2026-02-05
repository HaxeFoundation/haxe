package cs.system;

/** The exception that is thrown when an attempt to marshal an object across a context boundary fails. */
@:native("System.ContextMarshalException")
extern class ContextMarshalException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
