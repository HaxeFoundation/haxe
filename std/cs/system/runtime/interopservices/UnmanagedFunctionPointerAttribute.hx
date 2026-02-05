package cs.system.runtime.interopservices;

/** Controls the marshaling behavior of a delegate signature passed as an unmanaged function pointer to or from unmanaged code. This class cannot be inherited. */
@:native("System.Runtime.InteropServices.UnmanagedFunctionPointerAttribute")
extern class UnmanagedFunctionPointerAttribute extends cs.system.Attribute {
	/** Enables or disables best-fit mapping behavior when converting Unicode characters to ANSI characters. */
	var BestFitMapping:Bool;
	/** Indicates how to marshal string parameters to the method, and controls name mangling. */
	var CharSet:cs.system.runtime.interopservices.CharSet;
	/** Indicates whether the callee calls the  Windows API function before returning from the attributed method. */
	var SetLastError:Bool;
	/** Enables or disables the throwing of an exception on an unmappable Unicode character that is converted to an ANSI "?" character. */
	var ThrowOnUnmappableChar:Bool;
	/**
	 * Gets the value of the calling convention.
	 * @return The value of the calling convention specified by the  constructor.
	 */
	var CallingConvention(default, never):cs.system.runtime.interopservices.CallingConvention;
	function new(callingConvention:cs.system.runtime.interopservices.CallingConvention):Void;
}
