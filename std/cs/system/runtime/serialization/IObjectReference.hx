package cs.system.runtime.serialization;

/** Indicates that the current interface implementer is a reference to another object. */
@:native("System.Runtime.Serialization.IObjectReference")
extern interface IObjectReference {
	/**
	 * Returns the real object that should be deserialized, rather than the object that
	 * the serialized stream specifies.
	 * @param context The  from which the current object is deserialized.
	 * @return The actual object that is put into the graph.
	 */
	function GetRealObject(context:cs.system.runtime.serialization.StreamingContext):Dynamic;
}
