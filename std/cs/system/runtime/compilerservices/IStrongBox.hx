package cs.system.runtime.compilerservices;

/** Defines a property for accessing the value that an object references. */
@:native("System.Runtime.CompilerServices.IStrongBox")
extern interface IStrongBox {
	/**
	 * Gets or sets the value that an object references.
	 * @return The value that the object references.
	 */
	var Value(default, default):Dynamic;
}
