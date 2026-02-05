package cs.system;

/** The exception that is thrown when a requested method or operation is not implemented. */
@:native("System.NotImplementedException")
extern class NotImplementedException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
