package cs.system.runtime.interopservices;

/** Indicates that the attributed method is exposed by an unmanaged dynamic-link library (DLL) as a static entry point. */
@:native("System.Runtime.InteropServices.DllImportAttribute")
extern class DllImportAttribute extends cs.system.Attribute {
	/** Enables or disables best-fit mapping behavior when converting Unicode characters to ANSI characters. */
	var BestFitMapping:Bool;
	/** Indicates the calling convention of an entry point. */
	var CallingConvention:cs.system.runtime.interopservices.CallingConvention;
	/** Indicates how to marshal string parameters to the method and controls name mangling. */
	var CharSet:cs.system.runtime.interopservices.CharSet;
	/** Indicates the name or ordinal of the DLL entry point to be called. */
	var EntryPoint:String;
	/** Controls whether the  field causes the common language runtime to search an unmanaged DLL for entry-point names other than the one specified. */
	var ExactSpelling:Bool;
	/** Indicates whether unmanaged methods that have  or  return values are directly translated or whether  or  return values are automatically converted to exceptions. */
	var PreserveSig:Bool;
	/** Indicates whether the callee calls the  Windows API function before returning from the attributed method. */
	var SetLastError:Bool;
	/** Enables or disables the throwing of an exception on an unmappable Unicode character that is converted to an ANSI "?" character. */
	var ThrowOnUnmappableChar:Bool;
	/**
	 * Gets the name of the DLL file that contains the entry point.
	 * @return The name of the DLL file that contains the entry point.
	 */
	var Value(default, never):String;
	function new(dllName:String):Void;
}
