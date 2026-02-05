package cs.system.runtime.serialization;

/** When applied to a method, specifies that the method is called immediately after deserialization of an object in an object graph. The order of deserialization relative to other objects in the graph is non-deterministic. */
@:native("System.Runtime.Serialization.OnDeserializedAttribute")
extern class OnDeserializedAttribute extends cs.system.Attribute {
	function new():Void;
}
