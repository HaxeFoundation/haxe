package cs.system.runtime.serialization;

/** Implements a serialization surrogate selector that allows one object to perform serialization and deserialization of another. */
@:native("System.Runtime.Serialization.ISerializationSurrogate")
extern interface ISerializationSurrogate {
	/**
	 * Populates the provided  with the data needed to serialize the object.
	 * @param obj The object to serialize.
	 * @param info The  to populate with data.
	 * @param context The destination (see ) for this serialization.
	 */
	function GetObjectData(obj:Dynamic, info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Populates the object using the information in the .
	 * @param obj The object to populate.
	 * @param info The information to populate the object.
	 * @param context The source from which the object is deserialized.
	 * @param selector The surrogate selector where the search for a compatible
	 * surrogate begins.
	 * @return The populated deserialized object.
	 */
	function SetObjectData(obj:Dynamic, info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext, selector:cs.system.runtime.serialization.ISurrogateSelector):Dynamic;
}
