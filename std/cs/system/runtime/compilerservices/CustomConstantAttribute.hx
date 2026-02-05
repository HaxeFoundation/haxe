package cs.system.runtime.compilerservices;

/** Defines a constant value that a compiler can persist for a field or method parameter. */
@:native("System.Runtime.CompilerServices.CustomConstantAttribute")
extern class CustomConstantAttribute extends cs.system.Attribute {
	/**
	 * Gets the constant value stored by this attribute.
	 * @return The constant value stored by this attribute.
	 */
	var Value(default, never):Dynamic;
}
