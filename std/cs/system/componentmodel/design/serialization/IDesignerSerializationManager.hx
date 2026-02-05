package cs.system.componentmodel.design.serialization;

/** Provides an interface that can manage design-time serialization. */
@:native("System.ComponentModel.Design.Serialization.IDesignerSerializationManager")
extern interface IDesignerSerializationManager extends cs.system.IServiceProvider {
	/**
	 * Gets a stack-based, user-defined storage area that is useful for communication
	 * between serializers.
	 * @return A  that stores data.
	 */
	var Context(default, never):cs.system.componentmodel.design.serialization.ContextStack;
	/**
	 * Indicates custom properties that can be serializable with available serializers.
	 * @return A  containing the properties to be serialized.
	 */
	var Properties(default, never):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Adds the specified serialization provider to the serialization manager.
	 * @param provider The serialization provider to add.
	 */
	function AddSerializationProvider(provider:cs.system.componentmodel.design.serialization.IDesignerSerializationProvider):Void;
	/**
	 * Creates an instance of the specified type and adds it to a collection of named
	 * instances.
	 * @param type The data type to create.
	 * @param arguments The arguments to pass to the constructor for this type.
	 * @param name The name of the object. This name can be used to access the object
	 * later through . If  is passed, the object is still created but cannot be
	 * accessed by name.
	 * @param addToContainer If , this object is added to the design container. The
	 * object must implement  for this to have any effect.
	 * @return The newly created object instance.
	 */
	function CreateInstance(type:cs.system.Type, arguments:cs.system.collections.ICollection, name:String, addToContainer:Bool):Dynamic;
	/**
	 * Gets an instance of a created object of the specified name, or  if that object
	 * does not exist.
	 * @param name The name of the object to retrieve.
	 * @return An instance of the object with the given name, or  if no object by that
	 * name can be found.
	 */
	function GetInstance(name:String):Dynamic;
	/**
	 * Gets the name of the specified object, or  if the object has no name.
	 * @param value The object to retrieve the name for.
	 * @return The name of the object, or  if the object is unnamed.
	 */
	function GetName(value:Dynamic):String;
	/**
	 * Gets a serializer of the requested type for the specified object type.
	 * @param objectType The type of the object to get the serializer for.
	 * @param serializerType The type of the serializer to retrieve.
	 * @return An instance of the requested serializer, or  if no appropriate
	 * serializer can be located.
	 */
	function GetSerializer(objectType:cs.system.Type, serializerType:cs.system.Type):Dynamic;
	/**
	 * Gets a type of the specified name.
	 * @param typeName The fully qualified name of the type to load.
	 * @return An instance of the type, or  if the type cannot be loaded.
	 */
	function GetType(typeName:String):cs.system.Type;
	/**
	 * Removes a custom serialization provider from the serialization manager.
	 * @param provider The provider to remove. This object must have been added using .
	 */
	function RemoveSerializationProvider(provider:cs.system.componentmodel.design.serialization.IDesignerSerializationProvider):Void;
	/**
	 * Reports an error in serialization.
	 * @param errorInformation The error to report. This information object can be of
	 * any object type. If it is an exception, the message of the exception is
	 * extracted and reported to the user. If it is any other type,  is called to
	 * display the information to the user.
	 */
	function ReportError(errorInformation:Dynamic):Void;
	/**
	 * Sets the name of the specified existing object.
	 * @param instance The object instance to name.
	 * @param name The name to give the instance.
	 */
	function SetName(instance:Dynamic, name:String):Void;
}
