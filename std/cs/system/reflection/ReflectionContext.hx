package cs.system.reflection;

/** Represents a context that can provide reflection objects. */
@:native("System.Reflection.ReflectionContext")
extern class ReflectionContext {
	/**
	 * Gets the representation of the type of the specified object in this reflection
	 * context.
	 * @param value The object to represent.
	 * @return An object that represents the type of the specified object.
	 */
	function GetTypeForObject(value:Dynamic):cs.system.reflection.TypeInfo;
	/**
	 * Gets the representation, in this reflection context, of an assembly that is
	 * represented by an object from another reflection context.
	 * @param assembly The external representation of the assembly to represent in this
	 * context.
	 * @return The representation of the assembly in this reflection context.
	 */
	function MapAssembly(assembly:cs.system.reflection.Assembly):cs.system.reflection.Assembly;
	/**
	 * Gets the representation, in this reflection context, of a type represented by an
	 * object from another reflection context.
	 * @param type The external representation of the type to represent in this
	 * context.
	 * @return The representation of the type in this reflection context.
	 */
	function MapType(type:cs.system.reflection.TypeInfo):cs.system.reflection.TypeInfo;
}
