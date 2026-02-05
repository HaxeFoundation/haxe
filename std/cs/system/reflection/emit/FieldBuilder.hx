package cs.system.reflection.emit;

/** Defines and represents a field. This class cannot be inherited. */
@:native("System.Reflection.Emit.FieldBuilder")
extern class FieldBuilder extends cs.system.reflection.FieldInfo {
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns all the custom attributes defined for this field.
	 * @param inherit Controls inheritance of custom attributes from base classes.
	 * @return An array of type  representing all the custom attributes of the
	 * constructor represented by this  instance.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Retrieves the value of the field supported by the given object.
	 * @param obj The object on which to access the field.
	 * @return An  containing the value of the field reflected by this instance.
	 */
	function GetValue(obj:Dynamic):Dynamic;
	/**
	 * Indicates whether an attribute having the specified type is defined on a field.
	 * @param attributeType The type of the attribute.
	 * @param inherit Controls inheritance of custom attributes from base classes.
	 * @return if one or more instance of  is defined on this field; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Sets the default value of this field.
	 * @param defaultValue The new default value for this field.
	 */
	function SetConstant(defaultValue:Dynamic):Void;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Sets a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Specifies the field layout.
	 * @param iOffset The offset of the field within the type containing this field.
	 */
	function SetOffset(iOffset:Int):Void;
	/**
	 * Sets the value of the field supported by the given object.
	 * @param obj The object on which to access the field.
	 * @param val The value to assign to the field.
	 * @param invokeAttr A member of  that specifies the type of binding that is
	 * desired (for example, IBinder.CreateInstance, IBinder.ExactBinding).
	 * @param binder A set of properties and enabling for binding, coercion of argument
	 * types, and invocation of members using reflection. If binder is null, then
	 * IBinder.DefaultBinding is used.
	 * @param culture The software preferences of a particular culture.
	 */
	function SetValue(obj:Dynamic, val:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, culture:cs.system.globalization.CultureInfo):Void;
}
