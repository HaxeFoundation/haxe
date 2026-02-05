package cs.system.runtime.serialization;

/** Provides static methods to aid with the implementation of a  for serialization. This class cannot be inherited. */
@:native("System.Runtime.Serialization.FormatterServices")
extern class FormatterServices {
	/**
	 * Determines whether the specified  can be deserialized with the  property set to
	 * .
	 * @param t The  to check for the ability to deserialize.
	 * @param securityLevel The  property value.
	 */
	static function CheckTypeSecurity(t:cs.system.Type, securityLevel:cs.system.runtime.serialization.formatters.TypeFilterLevel):Void;
	/**
	 * Extracts the data from the specified object and returns it as an array of
	 * objects.
	 * @param obj The object to write to the formatter.
	 * @param members The members to extract from the object.
	 * @return An array of  that contains data stored in  and associated with .
	 */
	static function GetObjectData(obj:Dynamic, members:cs.NativeArray<cs.system.reflection.MemberInfo>):cs.NativeArray<Dynamic>;
	/**
	 * Creates a new instance of the specified object type.
	 * @param type The type of object to create.
	 * @return A zeroed object of the specified type.
	 */
	static function GetSafeUninitializedObject(type:cs.system.Type):Dynamic;
	@:overload(function(type:cs.system.Type):cs.NativeArray<cs.system.reflection.MemberInfo> {})
	/**
	 * Gets all the serializable members for a class of the specified .
	 * @param type The type being serialized.
	 * @return An array of type  of the non-transient, non-static members.
	 */
	static function GetSerializableMembers(type:cs.system.Type, context:cs.system.runtime.serialization.StreamingContext):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Returns a serialization surrogate for the specified .
	 * @param innerSurrogate The specified surrogate.
	 * @return An  for the specified .
	 */
	static function GetSurrogateForCyclicalReference(innerSurrogate:cs.system.runtime.serialization.ISerializationSurrogate):cs.system.runtime.serialization.ISerializationSurrogate;
	/**
	 * Looks up the  of the specified object in the provided .
	 * @param assem The assembly where you want to look up the object.
	 * @param name The name of the object.
	 * @return The  of the named object.
	 */
	static function GetTypeFromAssembly(assem:cs.system.reflection.Assembly, name:String):cs.system.Type;
	/**
	 * Creates a new instance of the specified object type.
	 * @param type The type of object to create.
	 * @return A zeroed object of the specified type.
	 */
	static function GetUninitializedObject(type:cs.system.Type):Dynamic;
	/**
	 * Populates the specified object with values for each field drawn from the data
	 * array of objects.
	 * @param obj The object to populate.
	 * @param members An array of  that describes which fields and properties to
	 * populate.
	 * @param data An array of  that specifies the values for each field and property
	 * to populate.
	 * @return The newly populated object.
	 */
	static function PopulateObjectMembers(obj:Dynamic, members:cs.NativeArray<cs.system.reflection.MemberInfo>, data:cs.NativeArray<Dynamic>):Dynamic;
}
