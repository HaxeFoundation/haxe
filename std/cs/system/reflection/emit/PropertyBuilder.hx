package cs.system.reflection.emit;

/** Defines the properties for a type. */
@:native("System.Reflection.Emit.PropertyBuilder")
extern class PropertyBuilder extends cs.system.reflection.PropertyInfo {
	/**
	 * Adds one of the other methods associated with this property.
	 * @param mdBuilder A  object that represents the other method.
	 */
	function AddOtherMethod(mdBuilder:cs.system.reflection.emit.MethodBuilder):Void;
	/**
	 * Returns an array of the public and non-public  and  accessors on this property.
	 * @param nonPublic Indicates whether non-public methods should be returned in the 
	 * array.  if non-public methods are to be included; otherwise, .
	 * @return An array of type  containing the matching public or non-public
	 * accessors, or an empty array if matching accessors do not exist on this
	 * property.
	 */
	function GetAccessors(nonPublic:Bool):cs.NativeArray<cs.system.reflection.MethodInfo>;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns an array of all the custom attributes for this property.
	 * @param inherit If , walks up this property's inheritance chain to find the
	 * custom attributes
	 * @return An array of all the custom attributes.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Returns the public and non-public get accessor for this property.
	 * @param nonPublic Indicates whether non-public get accessors should be returned. 
	 * if non-public methods are to be included; otherwise, .
	 * @return A  object representing the get accessor for this property, if  is .
	 * Returns  if  is  and the get accessor is non-public, or if  is  but no get
	 * accessors exist.
	 */
	function GetGetMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	/**
	 * Returns an array of all the index parameters for the property.
	 * @return An array of type  containing the parameters for the indexes.
	 */
	function GetIndexParameters():cs.NativeArray<cs.system.reflection.ParameterInfo>;
	/**
	 * Returns the set accessor for this property.
	 * @param nonPublic Indicates whether the accessor should be returned if it is
	 * non-public.  if non-public methods are to be included; otherwise, .
	 * @return The property's  method, or , as shown in the following table. Value
	 * Condition A  object representing the Set method for this property. The set
	 * accessor is public. is true and non-public methods can be returned. null is
	 * true, but the property is read-only. is false and the set accessor is
	 * non-public.
	 */
	function GetSetMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	@:overload(function(obj:Dynamic, index:cs.NativeArray<Dynamic>):Dynamic {})
	/**
	 * Gets the value of the indexed property by calling the property's getter method.
	 * @param obj The object whose property value will be returned.
	 * @param index Optional index values for indexed properties. This value should be 
	 * for non-indexed properties.
	 * @return The value of the specified indexed property.
	 */
	function GetValue(obj:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, index:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic;
	/**
	 * Indicates whether one or more instance of  is defined on this property.
	 * @param attributeType The  object to which the custom attributes are applied.
	 * @param inherit Specifies whether to walk up this property's inheritance chain to
	 * find the custom attributes.
	 * @return if one or more instance of  is defined on this property; otherwise .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Sets the default value of this property.
	 * @param defaultValue The default value of this property.
	 */
	function SetConstant(defaultValue:Dynamic):Void;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Set a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Sets the method that gets the property value.
	 * @param mdBuilder A  object that represents the method that gets the property
	 * value.
	 */
	function SetGetMethod(mdBuilder:cs.system.reflection.emit.MethodBuilder):Void;
	/**
	 * Sets the method that sets the property value.
	 * @param mdBuilder A  object that represents the method that sets the property
	 * value.
	 */
	function SetSetMethod(mdBuilder:cs.system.reflection.emit.MethodBuilder):Void;
	@:overload(function(obj:Dynamic, value:Dynamic, index:cs.NativeArray<Dynamic>):Void {})
	/**
	 * Sets the value of the property with optional index values for index properties.
	 * @param obj The object whose property value will be set.
	 * @param value The new value for this property.
	 * @param index Optional index values for indexed properties. This value should be 
	 * for non-indexed properties.
	 */
	function SetValue(obj:Dynamic, value:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, index:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Void;
}
