package cs.system.runtime.interopservices;

/** Controls whether Unicode characters are converted to the closest matching ANSI characters. */
@:native("System.Runtime.InteropServices.BestFitMappingAttribute")
extern class BestFitMappingAttribute extends cs.system.Attribute {
	/** Enables or disables the throwing of an exception on an unmappable Unicode character that is converted to an ANSI '?' character. */
	var ThrowOnUnmappableChar:Bool;
	/**
	 * Gets the best-fit mapping behavior when converting Unicode characters to ANSI
	 * characters.
	 * @return if best-fit mapping is enabled; otherwise, . The default is .
	 */
	var BestFitMapping(default, never):Bool;
	function new(BestFitMapping:Bool):Void;
}
