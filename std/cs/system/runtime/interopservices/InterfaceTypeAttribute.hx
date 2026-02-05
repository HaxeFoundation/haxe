package cs.system.runtime.interopservices;

/** Indicates whether a managed interface is dual, dispatch-only, or  -only when exposed to COM. */
@:native("System.Runtime.InteropServices.InterfaceTypeAttribute")
extern class InterfaceTypeAttribute extends cs.system.Attribute {
	/**
	 * Gets the  value that describes how the interface should be exposed to COM.
	 * @return The  value that describes how the interface should be exposed to COM.
	 */
	var Value(default, never):cs.system.runtime.interopservices.ComInterfaceType;
	@:overload(function(interfaceType:cs.Int16):Void {})
	function new(interfaceType:cs.system.runtime.interopservices.ComInterfaceType):Void;
}
