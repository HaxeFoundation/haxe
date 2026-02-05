package cs.system.componentmodel.design.serialization;

/** Provides an interface that can invoke serialization and deserialization. */
@:native("System.ComponentModel.Design.Serialization.IDesignerSerializationService")
extern interface IDesignerSerializationService {
	/**
	 * Deserializes the specified serialization data object and returns a collection of
	 * objects represented by that data.
	 * @param serializationData An object consisting of serialized data.
	 * @return An  of objects rebuilt from the specified serialization data object.
	 */
	function Deserialize(serializationData:Dynamic):cs.system.collections.ICollection;
	/**
	 * Serializes the specified collection of objects and stores them in a
	 * serialization data object.
	 * @param objects A collection of objects to serialize.
	 * @return An object that contains the serialized state of the specified collection
	 * of objects.
	 */
	function Serialize(objects:cs.system.collections.ICollection):Dynamic;
}
