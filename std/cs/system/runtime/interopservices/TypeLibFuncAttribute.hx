package cs.system.runtime.interopservices;

/** Contains the  that were originally imported for this method from the COM type library. */
@:native("System.Runtime.InteropServices.TypeLibFuncAttribute")
extern class TypeLibFuncAttribute extends cs.system.Attribute {
	/**
	 * Gets the  value for this method.
	 * @return The  value for this method.
	 */
	var Value(default, never):cs.system.runtime.interopservices.TypeLibFuncFlags;
	@:overload(function(flags:cs.Int16):Void {})
	function new(flags:cs.system.runtime.interopservices.TypeLibFuncFlags):Void;
}
