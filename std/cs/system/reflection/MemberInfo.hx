package cs.system.reflection;

/** Obtains information about the attributes of a member and provides access to member metadata. */
@:native("System.Reflection.MemberInfo")
extern class MemberInfo {
	/**
	 * Gets a collection that contains this member's custom attributes.
	 * @return A collection that contains this member's custom attributes.
	 */
	var CustomAttributes(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.CustomAttributeData>;
	/**
	 * Gets the class that declares this member.
	 * @return The  object for the class that declares this member.
	 */
	var DeclaringType(default, never):cs.system.Type;
	/**
	 * When overridden in a derived class, gets a  value indicating the type of the
	 * member - method, constructor, event, and so on.
	 * @return A  value indicating the type of member.
	 */
	var MemberType(default, never):cs.system.reflection.MemberTypes;
	/**
	 * Gets a value that identifies a metadata element.
	 * @return A value which, in combination with , uniquely identifies a metadata
	 * element.
	 */
	var MetadataToken(default, never):Int;
	/**
	 * Gets the module in which the type that declares the member represented by the
	 * current  is defined.
	 * @return The  in which the type that declares the member represented by the
	 * current  is defined.
	 */
	var Module(default, never):cs.system.reflection.Module;
	/**
	 * Gets the name of the current member.
	 * @return A  containing the name of this member.
	 */
	var Name(default, never):String;
	/**
	 * Gets the class object that was used to obtain this instance of .
	 * @return The  object through which this  object was obtained.
	 */
	var ReflectedType(default, never):cs.system.Type;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The  to compare to .
	 * @param right The  to compare to .
	 * @return if  is equal to ; otherwise .
	 */
	static function op_Equality(left:cs.system.reflection.MemberInfo, right:cs.system.reflection.MemberInfo):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The  to compare to .
	 * @param right The  to compare to .
	 * @return if  is not equal to ; otherwise .
	 */
	static function op_Inequality(left:cs.system.reflection.MemberInfo, right:cs.system.reflection.MemberInfo):Bool;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * When overridden in a derived class, returns an array of all custom attributes
	 * applied to this member.
	 * @param inherit to search this member's inheritance chain to find the attributes;
	 * otherwise, . This parameter is ignored for properties and events.
	 * @return An array that contains all the custom attributes applied to this member,
	 * or an array with zero elements if no attributes are defined.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Returns a list of  objects representing data about the attributes that have been
	 * applied to the target member.
	 * @return A generic list of  objects representing data about the attributes that
	 * have been applied to the target member.
	 */
	function GetCustomAttributesData():cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeData>;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/** @param other  */
	function HasSameMetadataDefinitionAs(other:cs.system.reflection.MemberInfo):Bool;
	/**
	 * When overridden in a derived class, indicates whether one or more attributes of
	 * the specified type or of its derived types is applied to this member.
	 * @param attributeType The type of custom attribute to search for. The search
	 * includes derived types.
	 * @param inherit to search this member's inheritance chain to find the attributes;
	 * otherwise, . This parameter is ignored for properties and events.
	 * @return if one or more instances of  or any of its derived types is applied to
	 * this member; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
}
