package cs.system.runtime.interopservices;

/** Specifies the COM dispatch identifier (DISPID) of a method, field, or property. */
@:native("System.Runtime.InteropServices.DispIdAttribute")
extern class DispIdAttribute extends cs.system.Attribute {
	/**
	 * Gets the DISPID for the member.
	 * @return The DISPID for the member.
	 */
	var Value(default, never):Int;
	function new(dispId:Int):Void;
}
