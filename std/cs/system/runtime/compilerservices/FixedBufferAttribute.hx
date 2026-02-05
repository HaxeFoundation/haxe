package cs.system.runtime.compilerservices;

/** Indicates that a field should be treated as containing a fixed number of elements of the specified primitive type. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.FixedBufferAttribute")
extern class FixedBufferAttribute extends cs.system.Attribute {
	/**
	 * Gets the type of the elements contained in the fixed buffer.
	 * @return The type of the elements.
	 */
	var ElementType(default, never):cs.system.Type;
	/**
	 * Gets the number of elements in the fixed buffer.
	 * @return The number of elements in the fixed buffer.
	 */
	var Length(default, never):Int;
	function new(elementType:cs.system.Type, length:Int):Void;
}
