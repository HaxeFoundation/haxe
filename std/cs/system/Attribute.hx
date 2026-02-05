package cs.system;

/** Represents the base class for custom attributes. */
@:native("System.Attribute")
extern class Attribute {
	/**
	 * When implemented in a derived class, gets a unique identifier for this .
	 * @return An  that is a unique identifier for the attribute.
	 */
	var TypeId(default, never):Dynamic;
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type, inherit:Bool):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type, inherit:Bool):cs.system.Attribute {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type, inherit:Bool):cs.system.Attribute {})
	/**
	 * Retrieves a custom attribute applied to a specified assembly. Parameters specify
	 * the assembly and the type of the custom attribute to search for.
	 * @param element An object derived from the  class that describes a reusable
	 * collection of modules.
	 * @param attributeType The type, or a base type, of the custom attribute to search
	 * for.
	 * @return A reference to the single custom attribute of type  that is applied to ,
	 * or  if there is no such attribute.
	 */
	static function GetCustomAttribute(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type, inherit:Bool):cs.system.Attribute;
	@:overload(function(element:cs.system.reflection.Assembly):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.MemberInfo):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Module):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.ParameterInfo):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Assembly, inherit:Bool):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.MemberInfo, inherit:Bool):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.MemberInfo, type:cs.system.Type):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Module, inherit:Bool):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.ParameterInfo, inherit:Bool):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.MemberInfo, type:cs.system.Type, inherit:Bool):cs.NativeArray<cs.system.Attribute> {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<cs.system.Attribute> {})
	/**
	 * Retrieves an array of the custom attributes applied to an assembly. A parameter
	 * specifies the assembly.
	 * @param element An object derived from the  class that describes a reusable
	 * collection of modules.
	 * @return An  array that contains the custom attributes applied to , or an empty
	 * array if no such custom attributes exist.
	 */
	static function GetCustomAttributes(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<cs.system.Attribute>;
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type):Bool {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type):Bool {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type):Bool {})
	@:overload(function(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type):Bool {})
	@:overload(function(element:cs.system.reflection.Assembly, attributeType:cs.system.Type, inherit:Bool):Bool {})
	@:overload(function(element:cs.system.reflection.MemberInfo, attributeType:cs.system.Type, inherit:Bool):Bool {})
	@:overload(function(element:cs.system.reflection.Module, attributeType:cs.system.Type, inherit:Bool):Bool {})
	/**
	 * Determines whether any custom attributes are applied to an assembly. Parameters
	 * specify the assembly, and the type of the custom attribute to search for.
	 * @param element An object derived from the  class that describes a reusable
	 * collection of modules.
	 * @param attributeType The type, or a base type, of the custom attribute to search
	 * for.
	 * @return if a custom attribute of type  is applied to ; otherwise, .
	 */
	static function IsDefined(element:cs.system.reflection.ParameterInfo, attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An  to compare with this instance or .
	 * @return if  and this instance are of the same type and have identical field
	 * values; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * When overridden in a derived class, indicates whether the value of this instance
	 * is the default value for the derived class.
	 * @return if this instance is the default attribute for the class; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
	/**
	 * When overridden in a derived class, returns a value that indicates whether this
	 * instance equals a specified object.
	 * @param obj An  to compare with this instance of .
	 * @return if this instance equals ; otherwise, .
	 */
	function Match(obj:Dynamic):Bool;
}
