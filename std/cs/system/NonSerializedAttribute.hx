package cs.system;

/** Indicates that a field of a serializable class should not be serialized. This class cannot be inherited. */
@:native("System.NonSerializedAttribute")
extern class NonSerializedAttribute extends cs.system.Attribute {
	function new():Void;
}
