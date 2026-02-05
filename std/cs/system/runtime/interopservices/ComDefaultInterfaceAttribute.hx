package cs.system.runtime.interopservices;

/** Specifies a default interface to expose to COM. This class cannot be inherited. */
@:native("System.Runtime.InteropServices.ComDefaultInterfaceAttribute")
extern class ComDefaultInterfaceAttribute extends cs.system.Attribute {
	/**
	 * Gets the  object that specifies the default interface to expose to COM.
	 * @return The  object that specifies the default interface to expose to COM.
	 */
	var Value(default, never):cs.system.Type;
	function new(defaultInterface:cs.system.Type):Void;
}
