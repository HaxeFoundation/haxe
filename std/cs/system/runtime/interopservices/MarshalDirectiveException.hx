package cs.system.runtime.interopservices;

/** The exception that is thrown by the marshaler when it encounters a  it does not support. */
@:native("System.Runtime.InteropServices.MarshalDirectiveException")
extern class MarshalDirectiveException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
