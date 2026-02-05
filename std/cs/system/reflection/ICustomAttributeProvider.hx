package cs.system.reflection;

/** Provides custom attributes for reflection objects that support them. */
@:native("System.Reflection.ICustomAttributeProvider")
extern interface ICustomAttributeProvider {
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns an array of all of the custom attributes defined on this member,
	 * excluding named attributes, or an empty array if there are no custom attributes.
	 * @param inherit When , look up the hierarchy chain for the inherited custom
	 * attribute.
	 * @return An array of Objects representing custom attributes, or an empty array.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Indicates whether one or more instance of  is defined on this member.
	 * @param attributeType The type of the custom attributes.
	 * @param inherit When , look up the hierarchy chain for the inherited custom
	 * attribute.
	 * @return if the  is defined on this member;  otherwise.
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
}
