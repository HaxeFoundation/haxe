package cs.system.runtime.interopservices;

/** The exception thrown when an invalid COM object is used. */
@:native("System.Runtime.InteropServices.InvalidComObjectException")
extern class InvalidComObjectException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
