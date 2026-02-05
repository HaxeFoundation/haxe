package cs.system.runtime.interopservices;

/** The base exception type for all COM interop exceptions and structured exception handling (SEH) exceptions. */
@:native("System.Runtime.InteropServices.ExternalException")
extern class ExternalException extends cs.system.SystemException {
	/**
	 * Gets the  of the error.
	 * @return The  of the error.
	 */
	var ErrorCode(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(message:String, errorCode:Int):Void;
	/**
	 * Returns a string that contains the HRESULT of the error.
	 * @return A string that represents the HRESULT.
	 */
	function ToString():String;
}
