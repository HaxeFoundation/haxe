package cs.system.reflection;

/** Discovers the attributes of a property and provides access to property metadata. */
@:native("System.Reflection.PropertyInfo")
extern class PropertyInfo extends cs.system.reflection.MemberInfo {
	/**
	 * Gets the attributes for this property.
	 * @return The attributes of this property.
	 */
	var Attributes(default, never):cs.system.reflection.PropertyAttributes;
	/**
	 * Gets a value indicating whether the property can be read.
	 * @return if this property can be read; otherwise, .
	 */
	var CanRead(default, never):Bool;
	/**
	 * Gets a value indicating whether the property can be written to.
	 * @return if this property can be written to; otherwise, .
	 */
	var CanWrite(default, never):Bool;
	/**
	 * Gets the  accessor for this property.
	 * @return The  accessor for this property.
	 */
	var GetMethod(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets a value indicating whether the property is the special name.
	 * @return if this property is the special name; otherwise, .
	 */
	var IsSpecialName(default, never):Bool;
	/**
	 * Gets the type of this property.
	 * @return The type of this property.
	 */
	var PropertyType(default, never):cs.system.Type;
	/**
	 * Gets the  accessor for this property.
	 * @return The  accessor for this property, or  if the property is read-only.
	 */
	var SetMethod(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.PropertyInfo, right:cs.system.reflection.PropertyInfo):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.PropertyInfo, right:cs.system.reflection.PropertyInfo):Bool;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	@:overload(function():cs.NativeArray<cs.system.reflection.MethodInfo> {})
	/**
	 * Returns an array whose elements reflect the public  and  accessors of the
	 * property reflected by the current instance.
	 * @return An array of  objects that reflect the public  and  accessors of the
	 * property reflected by the current instance, if found; otherwise, this method
	 * returns an array with zero (0) elements.
	 */
	function GetAccessors(nonPublic:Bool):cs.NativeArray<cs.system.reflection.MethodInfo>;
	/**
	 * Returns a literal value associated with the property by a compiler.
	 * @return An  that contains the literal value associated with the property. If the
	 * literal value is a class type with an element value of zero, the return value is
	 * .
	 */
	function GetConstantValue():Dynamic;
	@:overload(function():cs.system.reflection.MethodInfo {})
	/**
	 * Returns the public  accessor for this property.
	 * @return A  object representing the public  accessor for this property, or  if
	 * the  accessor is non-public or does not exist.
	 */
	function GetGetMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * When overridden in a derived class, returns an array of all the index parameters
	 * for the property.
	 * @return An array of type  containing the parameters for the indexes. If the
	 * property is not indexed, the array has 0 (zero) elements.
	 */
	function GetIndexParameters():cs.NativeArray<cs.system.reflection.ParameterInfo>;
	/**
	 * Returns an array of types representing the optional custom modifiers of the
	 * property.
	 * @return An array of  objects that identify the optional custom modifiers of the
	 * current property, such as  or .
	 */
	function GetOptionalCustomModifiers():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns a literal value associated with the property by a compiler.
	 * @return An  that contains the literal value associated with the property. If the
	 * literal value is a class type with an element value of zero, the return value is
	 * .
	 */
	function GetRawConstantValue():Dynamic;
	/**
	 * Returns an array of types representing the required custom modifiers of the
	 * property.
	 * @return An array of  objects that identify the required custom modifiers of the
	 * current property, such as  or .
	 */
	function GetRequiredCustomModifiers():cs.NativeArray<cs.system.Type>;
	@:overload(function():cs.system.reflection.MethodInfo {})
	/**
	 * Returns the public  accessor for this property.
	 * @return The  object representing the  method for this property if the  accessor
	 * is public, or  if the  accessor is not public.
	 */
	function GetSetMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	@:overload(function(obj:Dynamic):Dynamic {})
	@:overload(function(obj:Dynamic, index:cs.NativeArray<Dynamic>):Dynamic {})
	/**
	 * Returns the property value of a specified object.
	 * @param obj The object whose property value will be returned.
	 * @return The property value of the specified object.
	 */
	function GetValue(obj:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, index:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic;
	@:overload(function(obj:Dynamic, value:Dynamic):Void {})
	@:overload(function(obj:Dynamic, value:Dynamic, index:cs.NativeArray<Dynamic>):Void {})
	/**
	 * Sets the property value of a specified object.
	 * @param obj The object whose property value will be set.
	 * @param value The new property value.
	 */
	function SetValue(obj:Dynamic, value:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, index:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Void;
}
