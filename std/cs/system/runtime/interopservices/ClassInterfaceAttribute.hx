package cs.system.runtime.interopservices;

/** Indicates the type of class interface to be generated for a class exposed to COM, if an interface is generated at all. */
@:native("System.Runtime.InteropServices.ClassInterfaceAttribute")
extern class ClassInterfaceAttribute extends cs.system.Attribute {
	/**
	 * Gets the  value that describes which type of interface should be generated for
	 * the class.
	 * @return The  value that describes which type of interface should be generated
	 * for the class.
	 */
	var Value(default, never):cs.system.runtime.interopservices.ClassInterfaceType;
	@:overload(function(classInterfaceType:cs.Int16):Void {})
	function new(classInterfaceType:cs.system.runtime.interopservices.ClassInterfaceType):Void;
}
