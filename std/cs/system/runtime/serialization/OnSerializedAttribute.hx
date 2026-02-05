package cs.system.runtime.serialization;

/** When applied to a method, specifies that the method is called after serialization of an object in an object graph. The order of serialization relative to other objects in the graph is non-deterministic. */
@:native("System.Runtime.Serialization.OnSerializedAttribute")
extern class OnSerializedAttribute extends cs.system.Attribute {
	function new():Void;
}
