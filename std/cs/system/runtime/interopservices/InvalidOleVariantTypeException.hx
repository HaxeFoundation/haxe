package cs.system.runtime.interopservices;

/** The exception thrown by the marshaler when it encounters an argument of a variant type that can not be marshaled to managed code. */
@:native("System.Runtime.InteropServices.InvalidOleVariantTypeException")
extern class InvalidOleVariantTypeException extends cs.system.SystemException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
}
