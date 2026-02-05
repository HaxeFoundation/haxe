package cs.system.runtime.interopservices;

/** Contains the  that were originally imported for this type from the COM type library. */
@:native("System.Runtime.InteropServices.TypeLibTypeAttribute")
extern class TypeLibTypeAttribute extends cs.system.Attribute {
	/**
	 * Gets the  value for this type.
	 * @return The  value for this type.
	 */
	var Value(default, never):cs.system.runtime.interopservices.TypeLibTypeFlags;
	@:overload(function(flags:cs.Int16):Void {})
	function new(flags:cs.system.runtime.interopservices.TypeLibTypeFlags):Void;
}
