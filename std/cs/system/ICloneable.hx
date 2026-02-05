package cs.system;

/** Supports cloning, which creates a new instance of a class with the same value as an existing instance. */
@:native("System.ICloneable")
extern interface ICloneable {
	/**
	 * Creates a new object that is a copy of the current instance.
	 * @return A new object that is a copy of this instance.
	 */
	function Clone():Dynamic;
}
