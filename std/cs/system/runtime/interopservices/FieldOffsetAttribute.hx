package cs.system.runtime.interopservices;

/** Indicates the physical position of fields within the unmanaged representation of a class or structure. */
@:native("System.Runtime.InteropServices.FieldOffsetAttribute")
extern class FieldOffsetAttribute extends cs.system.Attribute {
	/**
	 * Gets the offset from the beginning of the structure to the beginning of the
	 * field.
	 * @return The offset from the beginning of the structure to the beginning of the
	 * field.
	 */
	var Value(default, never):Int;
	function new(offset:Int):Void;
}
