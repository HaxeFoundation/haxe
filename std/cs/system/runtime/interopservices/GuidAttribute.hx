package cs.system.runtime.interopservices;

/** Supplies an explicit  when an automatic GUID is undesirable. */
@:native("System.Runtime.InteropServices.GuidAttribute")
extern class GuidAttribute extends cs.system.Attribute {
	/**
	 * Gets the  of the class.
	 * @return The  of the class.
	 */
	var Value(default, never):String;
	function new(guid:String):Void;
}
