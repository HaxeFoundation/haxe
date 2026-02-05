package cs.system.reflection;

/** Contains static methods for retrieving custom attributes. */
@:native("System.Reflection.CustomAttributeExtensions")
extern class CustomAttributeExtensions {
	@:overload(function<T>(element:cs.system.reflection.Assembly):T {})
	@:overload(function<T>(element:cs.system.reflection.MemberInfo):T {})
	@:overload(function<T>(element:cs.system.reflection.Module):T {})
	@:overload(function<T>(element:cs.system.reflection.ParameterInfo):T {})
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type):cs.system.Attribute {})
	@:overload(function<T>(element:cs.system.reflection.MemberInfo, inherit:Bool):T {})
	@:overload(function<T>(element:cs.system.reflection.ParameterInfo, inherit:Bool):T {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type, inherit:Bool):cs.system.Attribute {})
	/**
	 * Retrieves a custom attribute of a specified type that is applied to a specified
	 * assembly.
	 * @param element The assembly to inspect.
	 * @param attributeType The type of attribute to search for.
	 * @return A custom attribute that matches , or  if no such attribute is found.
	 */
	static function GetCustomAttribute(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type, inherit:Bool):cs.system.Attribute;
	@:overload(function(element:cs.system.reflection.Assembly):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.MemberInfo):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Module):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.ParameterInfo):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function<T>(element:cs.system.reflection.Assembly):cs.system.collections.generic.IEnumerable<T> {})
	@:overload(function<T>(element:cs.system.reflection.MemberInfo):cs.system.collections.generic.IEnumerable<T> {})
	@:overload(function<T>(element:cs.system.reflection.Module):cs.system.collections.generic.IEnumerable<T> {})
	@:overload(function<T>(element:cs.system.reflection.ParameterInfo):cs.system.collections.generic.IEnumerable<T> {})
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.MemberInfo, inherit:Bool):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.ParameterInfo, inherit:Bool):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	@:overload(function<T>(element:cs.system.reflection.MemberInfo, inherit:Bool):cs.system.collections.generic.IEnumerable<T> {})
	@:overload(function<T>(element:cs.system.reflection.ParameterInfo, inherit:Bool):cs.system.collections.generic.IEnumerable<T> {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type, inherit:Bool):cs.system.collections.generic.IEnumerable<cs.system.Attribute> {})
	/**
	 * Retrieves a collection of custom attributes that are applied to a specified
	 * assembly.
	 * @param element The assembly to inspect.
	 * @return A collection of the custom attributes that are applied to , or an empty
	 * collection if no such attributes exist.
	 */
	static function GetCustomAttributes(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type, inherit:Bool):cs.system.collections.generic.IEnumerable<cs.system.Attribute>;
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type):Bool {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type):Bool {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type):Bool {})
	@:overload(function(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type):Bool {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type, inherit:Bool):Bool {})
	/**
	 * Indicates whether custom attributes of a specified type are applied to a
	 * specified assembly.
	 * @param element The assembly to inspect.
	 * @param attributeType The type of the attribute to search for.
	 * @return if an attribute of the specified type is applied to ; otherwise, .
	 */
	static function IsDefined(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type, inherit:Bool):Bool;
}
