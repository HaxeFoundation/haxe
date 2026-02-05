package cs.system.reflection;

/** Represents a missing . This class cannot be inherited. */
@:native("System.Reflection.Missing")
extern class Missing {
	/** Represents the sole instance of the  class. */
	static var Value(default, never):cs.system.reflection.Missing;
}
