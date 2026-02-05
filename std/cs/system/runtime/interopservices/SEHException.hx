package cs.system.runtime.interopservices;

/** Represents structured exception handling (SEH) errors. */
@:native("System.Runtime.InteropServices.SEHException")
extern class SEHException extends cs.system.runtime.interopservices.ExternalException {
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, inner:cs.system.Exception):Void;
	/**
	 * Indicates whether the exception can be recovered from, and whether the code can
	 * continue from the point at which the exception was thrown.
	 * @return Always , because resumable exceptions are not implemented.
	 */
	function CanResume():Bool;
}
