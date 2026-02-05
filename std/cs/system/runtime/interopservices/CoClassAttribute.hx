package cs.system.runtime.interopservices;

/** Specifies the class identifier of a coclass imported from a type library. */
@:native("System.Runtime.InteropServices.CoClassAttribute")
extern class CoClassAttribute extends cs.system.Attribute {
	/**
	 * Gets the class identifier of the original coclass.
	 * @return A  containing the class identifier of the original coclass.
	 */
	var CoClass(default, never):cs.system.Type;
	function new(coClass:cs.system.Type):Void;
}
