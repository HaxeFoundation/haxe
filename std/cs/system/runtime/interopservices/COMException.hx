package cs.system.runtime.interopservices;

/** The exception that is thrown when an unrecognized HRESULT is returned from a COM method call. */
@:native("System.Runtime.InteropServices.COMException")
extern class COMException extends cs.system.runtime.interopservices.ExternalException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(message:String, errorCode:Int):Void;
	/**
	 * Converts the contents of the exception to a string.
	 * @return A string containing the , , , and  properties of the exception.
	 */
	function ToString():String;
}
