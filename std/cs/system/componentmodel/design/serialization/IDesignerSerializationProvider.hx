package cs.system.componentmodel.design.serialization;

/** Provides an interface that enables access to a serializer. */
@:native("System.ComponentModel.Design.Serialization.IDesignerSerializationProvider")
extern interface IDesignerSerializationProvider {
	/**
	 * Gets a serializer using the specified attributes.
	 * @param manager The serialization manager requesting the serializer.
	 * @param currentSerializer An instance of the current serializer of the specified
	 * type. This can be  if no serializer of the specified type exists.
	 * @param objectType The data type of the object to serialize.
	 * @param serializerType The data type of the serializer to create.
	 * @return An instance of a serializer of the type requested, or  if the request
	 * cannot be satisfied.
	 */
	function GetSerializer(manager:cs.system.componentmodel.design.serialization.IDesignerSerializationManager, currentSerializer:Dynamic, objectType:cs.system.Type, serializerType:cs.system.Type):Dynamic;
}
