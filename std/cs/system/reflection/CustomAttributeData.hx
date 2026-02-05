package cs.system.reflection;

/** Provides access to custom attribute data for assemblies, modules, types, members and parameters that are loaded into the reflection-only context. */
@:native("System.Reflection.CustomAttributeData")
extern class CustomAttributeData {
	/**
	 * Gets the type of the attribute.
	 * @return The type of the attribute.
	 */
	var AttributeType(default, never):cs.system.Type;
	/**
	 * Gets a  object that represents the constructor that would have initialized the
	 * custom attribute.
	 * @return An object that represents the constructor that would have initialized
	 * the custom attribute represented by the current instance of the  class.
	 */
	var Constructor(default, never):cs.system.reflection.ConstructorInfo;
	/**
	 * Gets the list of positional arguments specified for the attribute instance
	 * represented by the  object.
	 * @return A collection of structures that represent the positional arguments
	 * specified for the custom attribute instance.
	 */
	var ConstructorArguments(default, never):cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeTypedArgument>;
	/**
	 * Gets the list of named arguments specified for the attribute instance
	 * represented by the  object.
	 * @return A collection of structures that represent the named arguments specified
	 * for the custom attribute instance.
	 */
	var NamedArguments(default, never):cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeNamedArgument>;
	@:overload(function(target:cs.system.reflection.Assembly):cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeData> {})
	@:overload(function(target:cs.system.reflection.MemberInfo):cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeData> {})
	@:overload(function(target:cs.system.reflection.Module):cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeData> {})
	/**
	 * Returns a list of  objects representing data about the attributes that have been
	 * applied to the target assembly.
	 * @param target The assembly whose custom attribute data is to be retrieved.
	 * @return A list of objects that represent data about the attributes that have
	 * been applied to the target assembly.
	 */
	static function GetCustomAttributes(target:cs.system.reflection.ParameterInfo):cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeData>;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  is equal to the current instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for a particular type.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string representation of the custom attribute.
	 * @return A string value that represents the custom attribute.
	 */
	function ToString():String;
}
