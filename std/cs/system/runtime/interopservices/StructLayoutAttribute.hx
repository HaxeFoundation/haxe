package cs.system.runtime.interopservices;

/** Lets you control the physical layout of the data fields of a class or structure in memory. */
@:native("System.Runtime.InteropServices.StructLayoutAttribute")
extern class StructLayoutAttribute extends cs.system.Attribute {
	/** Indicates whether string data fields within the class should be marshaled as  or  by default. */
	var CharSet:cs.system.runtime.interopservices.CharSet;
	/** Controls the alignment of data fields of a class or structure in memory. */
	var Pack:Int;
	/** Indicates the absolute size of the class or structure. */
	var Size:Int;
	/**
	 * Gets the  value that specifies how the class or structure is arranged.
	 * @return One of the enumeration values that specifies how the class or structure
	 * is arranged.
	 */
	var Value(default, never):cs.system.runtime.interopservices.LayoutKind;
	@:overload(function(layoutKind:cs.Int16):Void {})
	function new(layoutKind:cs.system.runtime.interopservices.LayoutKind):Void;
}
