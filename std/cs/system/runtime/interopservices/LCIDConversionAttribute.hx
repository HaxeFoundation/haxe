package cs.system.runtime.interopservices;

/** Indicates that a method's unmanaged signature expects a locale identifier (LCID) parameter. */
@:native("System.Runtime.InteropServices.LCIDConversionAttribute")
extern class LCIDConversionAttribute extends cs.system.Attribute {
	/**
	 * Gets the position of the LCID argument in the unmanaged signature.
	 * @return The position of the LCID argument in the unmanaged signature, where 0 is
	 * the first argument.
	 */
	var Value(default, never):Int;
	function new(lcid:Int):Void;
}
