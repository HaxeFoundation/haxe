package cs.system.runtime.serialization;

/** When applied to a method, specifies that the method is called during deserialization of an object in an object graph. The order of deserialization relative to other objects in the graph is non-deterministic. */
@:native("System.Runtime.Serialization.OnDeserializingAttribute")
extern class OnDeserializingAttribute extends cs.system.Attribute {
	function new():Void;
}
