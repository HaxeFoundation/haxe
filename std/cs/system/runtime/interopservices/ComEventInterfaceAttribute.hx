package cs.system.runtime.interopservices;

/** Identifies the source interface and the class that implements the methods of the event interface that is generated when a coclass is imported from a COM type library. */
@:native("System.Runtime.InteropServices.ComEventInterfaceAttribute")
extern class ComEventInterfaceAttribute extends cs.system.Attribute {
	/**
	 * Gets the class that implements the methods of the event interface.
	 * @return A  that contains the class that implements the methods of the event
	 * interface.
	 */
	var EventProvider(default, never):cs.system.Type;
	/**
	 * Gets the original source interface from the type library.
	 * @return A  containing the source interface.
	 */
	var SourceInterface(default, never):cs.system.Type;
	function new(SourceInterface:cs.system.Type, EventProvider:cs.system.Type):Void;
}
