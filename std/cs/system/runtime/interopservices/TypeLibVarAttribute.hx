package cs.system.runtime.interopservices;

/** Contains the  that were originally imported for this field from the COM type library. */
@:native("System.Runtime.InteropServices.TypeLibVarAttribute")
extern class TypeLibVarAttribute extends cs.system.Attribute {
	/**
	 * Gets the  value for this field.
	 * @return The  value for this field.
	 */
	var Value(default, never):cs.system.runtime.interopservices.TypeLibVarFlags;
	@:overload(function(flags:cs.Int16):Void {})
	function new(flags:cs.system.runtime.interopservices.TypeLibVarFlags):Void;
}
