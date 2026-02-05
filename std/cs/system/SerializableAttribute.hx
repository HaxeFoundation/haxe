package cs.system;

/** Indicates that a class can be serialized. This class cannot be inherited. */
@:native("System.SerializableAttribute")
extern class SerializableAttribute extends cs.system.Attribute {
	function new():Void;
}
