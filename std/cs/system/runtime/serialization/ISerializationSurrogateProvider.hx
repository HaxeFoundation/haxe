package cs.system.runtime.serialization;

/** Provides the methods needed to construct a serialization surrogate that extends the . A serialization surrogate is used during serialization and deserialization to substitute one type for another. */
@:native("System.Runtime.Serialization.ISerializationSurrogateProvider")
extern interface ISerializationSurrogateProvider {
	/**
	 * During deserialization, returns an object that is a substitute for the specified
	 * object.
	 * @param obj The deserialized object to be substituted.
	 * @param targetType The  that the substituted object should be assigned to.
	 * @return The substituted deserialized object.
	 */
	function GetDeserializedObject(obj:Dynamic, targetType:cs.system.Type):Dynamic;
	/**
	 * During serialization, returns an object that substitutes the specified object.
	 * @param obj The object to substitute.
	 * @param targetType The  that the substituted object should be assigned to.
	 * @return The substituted object that will be serialized.
	 */
	function GetObjectToSerialize(obj:Dynamic, targetType:cs.system.Type):Dynamic;
	/**
	 * During serialization, deserialization, and schema import and export, returns a
	 * data contract type that substitutes the specified type.
	 * @param type The type to substitute.
	 * @return The  to substitute for the  value.
	 */
	function GetSurrogateType(type:cs.system.Type):cs.system.Type;
}
