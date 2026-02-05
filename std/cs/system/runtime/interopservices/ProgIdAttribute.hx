package cs.system.runtime.interopservices;

/** Allows the user to specify the ProgID of a class. */
@:native("System.Runtime.InteropServices.ProgIdAttribute")
extern class ProgIdAttribute extends cs.system.Attribute {
	/**
	 * Gets the ProgID of the class.
	 * @return The ProgID of the class.
	 */
	var Value(default, never):String;
	function new(progId:String):Void;
}
