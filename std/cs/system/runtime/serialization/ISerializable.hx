package cs.system.runtime.serialization;

/** Allows an object to control its own serialization and deserialization. */
@:native("System.Runtime.Serialization.ISerializable")
extern interface ISerializable {
	/**
	 * Populates a  with the data needed to serialize the target object.
	 * @param info The  to populate with data.
	 * @param context The destination (see ) for this serialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
