package cs.system.reflection;

/** Discovers the attributes of a parameter and provides access to parameter metadata. */
@:native("System.Reflection.ParameterInfo")
extern class ParameterInfo {
	/**
	 * Gets the attributes for this parameter.
	 * @return A  object representing the attributes for this parameter.
	 */
	var Attributes(default, never):cs.system.reflection.ParameterAttributes;
	/**
	 * Gets a collection that contains this parameter's custom attributes.
	 * @return A collection that contains this parameter's custom attributes.
	 */
	var CustomAttributes(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.CustomAttributeData>;
	/**
	 * Gets a value indicating the default value if the parameter has a default value.
	 * @return The default value of the parameter, or  if the parameter has no default
	 * value.
	 */
	var DefaultValue(default, never):Dynamic;
	/**
	 * Gets a value that indicates whether this parameter has a default value.
	 * @return if this parameter has a default value; otherwise, .
	 */
	var HasDefaultValue(default, never):Bool;
	/**
	 * Gets a value indicating whether this is an input parameter.
	 * @return if the parameter is an input parameter; otherwise, .
	 */
	var IsIn(default, never):Bool;
	/**
	 * Gets a value indicating whether this parameter is a locale identifier (lcid).
	 * @return if the parameter is a locale identifier; otherwise, .
	 */
	var IsLcid(default, never):Bool;
	/**
	 * Gets a value indicating whether this parameter is optional.
	 * @return if the parameter is optional; otherwise, .
	 */
	var IsOptional(default, never):Bool;
	/**
	 * Gets a value indicating whether this is an output parameter.
	 * @return if the parameter is an output parameter; otherwise, .
	 */
	var IsOut(default, never):Bool;
	/**
	 * Gets a value indicating whether this is a  parameter.
	 * @return if the parameter is a ; otherwise, .
	 */
	var IsRetval(default, never):Bool;
	/**
	 * Gets a value indicating the member in which the parameter is implemented.
	 * @return The member which implanted the parameter represented by this .
	 */
	var Member(default, never):cs.system.reflection.MemberInfo;
	/**
	 * Gets a value that identifies this parameter in metadata.
	 * @return A value which, in combination with the module, uniquely identifies this
	 * parameter in metadata.
	 */
	var MetadataToken(default, never):Int;
	/**
	 * Gets the name of the parameter.
	 * @return The simple name of this parameter.
	 */
	var Name(default, never):String;
	/**
	 * Gets the  of this parameter.
	 * @return The  object that represents the  of this parameter.
	 */
	var ParameterType(default, never):cs.system.Type;
	/**
	 * Gets the zero-based position of the parameter in the formal parameter list.
	 * @return An integer representing the position this parameter occupies in the
	 * parameter list.
	 */
	var Position(default, never):Int;
	/**
	 * Gets a value indicating the default value if the parameter has a default value.
	 * @return The default value of the parameter, or  if the parameter has no default
	 * value.
	 */
	var RawDefaultValue(default, never):Dynamic;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Gets all the custom attributes defined on this parameter.
	 * @param inherit This argument is ignored for objects of this type.
	 * @return An array that contains all the custom attributes applied to this
	 * parameter.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Returns a list of  objects for the current parameter, which can be used in the
	 * reflection-only context.
	 * @return A generic list of  objects representing data about the attributes that
	 * have been applied to the current parameter.
	 */
	function GetCustomAttributesData():cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeData>;
	/**
	 * Gets the optional custom modifiers of the parameter.
	 * @return An array of  objects that identify the optional custom modifiers of the
	 * current parameter, such as  or .
	 */
	function GetOptionalCustomModifiers():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns the real object that should be deserialized instead of the object that
	 * the serialized stream specifies.
	 * @param context The serialized stream from which the current object is
	 * deserialized.
	 * @return The actual object that is put into the graph.
	 */
	function GetRealObject(context:cs.system.runtime.serialization.StreamingContext):Dynamic;
	/**
	 * Gets the required custom modifiers of the parameter.
	 * @return An array of  objects that identify the required custom modifiers of the
	 * current parameter, such as  or .
	 */
	function GetRequiredCustomModifiers():cs.NativeArray<cs.system.Type>;
	/**
	 * Determines whether the custom attribute of the specified type or its derived
	 * types is applied to this parameter.
	 * @param attributeType The  object to search for.
	 * @param inherit This argument is ignored for objects of this type.
	 * @return if one or more instances of  or its derived types are applied to this
	 * parameter; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Gets the parameter type and name represented as a string.
	 * @return A string containing the type and the name of the parameter.
	 */
	function ToString():String;
}
