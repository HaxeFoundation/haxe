package cs.system.runtime.interopservices;

/** Wraps objects the marshaler should marshal as a . */
@:native("System.Runtime.InteropServices.ErrorWrapper")
extern class ErrorWrapper {
	/**
	 * Gets the error code of the wrapper.
	 * @return The HRESULT of the error.
	 */
	var ErrorCode(default, never):Int;
	@:overload(function(e:cs.system.Exception):Void {})
	@:overload(function(errorCode:Int):Void {})
	function new(errorCode:Dynamic):Void;
}
