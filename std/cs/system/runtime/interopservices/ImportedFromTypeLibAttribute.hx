package cs.system.runtime.interopservices;

/** Indicates that the types defined within an assembly were originally defined in a type library. */
@:native("System.Runtime.InteropServices.ImportedFromTypeLibAttribute")
extern class ImportedFromTypeLibAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of the original type library file.
	 * @return The name of the original type library file.
	 */
	var Value(default, never):String;
	function new(tlbFile:String):Void;
}
