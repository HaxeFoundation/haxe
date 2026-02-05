package cs.system.reflection;

/** Discovers the attributes of a field and provides access to field metadata. */
@:native("System.Reflection.FieldInfo")
extern class FieldInfo extends cs.system.reflection.MemberInfo {
	/**
	 * Gets the attributes associated with this field.
	 * @return The  for this field.
	 */
	var Attributes(default, never):cs.system.reflection.FieldAttributes;
	/**
	 * Gets a , which is a handle to the internal metadata representation of a field.
	 * @return A handle to the internal metadata representation of a field.
	 */
	var FieldHandle(default, never):cs.system.RuntimeFieldHandle;
	/**
	 * Gets the type of this field object.
	 * @return The type of this field object.
	 */
	var FieldType(default, never):cs.system.Type;
	/**
	 * Gets a value indicating whether the potential visibility of this field is
	 * described by ; that is, the field is visible at most to other types in the same
	 * assembly, and is not visible to derived types outside the assembly.
	 * @return if the visibility of this field is exactly described by ; otherwise, .
	 */
	var IsAssembly(default, never):Bool;
	/**
	 * Gets a value indicating whether the visibility of this field is described by ;
	 * that is, the field is visible only within its class and derived classes.
	 * @return if access to this field is exactly described by ; otherwise, .
	 */
	var IsFamily(default, never):Bool;
	/**
	 * Gets a value indicating whether the visibility of this field is described by ;
	 * that is, the field can be accessed from derived classes, but only if they are in
	 * the same assembly.
	 * @return if access to this field is exactly described by ; otherwise, .
	 */
	var IsFamilyAndAssembly(default, never):Bool;
	/**
	 * Gets a value indicating whether the potential visibility of this field is
	 * described by ; that is, the field can be accessed by derived classes wherever
	 * they are, and by classes in the same assembly.
	 * @return if access to this field is exactly described by ; otherwise, .
	 */
	var IsFamilyOrAssembly(default, never):Bool;
	/**
	 * Gets a value indicating whether the field can only be set in the body of the
	 * constructor.
	 * @return if the field has the  attribute set; otherwise, .
	 */
	var IsInitOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether the value is written at compile time and cannot
	 * be changed.
	 * @return if the field has the  attribute set; otherwise, .
	 */
	var IsLiteral(default, never):Bool;
	/**
	 * Gets a value indicating whether this field has the  attribute.
	 * @return if the field has the  attribute set; otherwise, .
	 */
	var IsNotSerialized(default, never):Bool;
	/**
	 * Gets a value indicating whether the corresponding  attribute is set in .
	 * @return if the  attribute is set in ; otherwise, .
	 */
	var IsPinvokeImpl(default, never):Bool;
	/**
	 * Gets a value indicating whether the field is private.
	 * @return if the field is private; otherwise; .
	 */
	var IsPrivate(default, never):Bool;
	/**
	 * Gets a value indicating whether the field is public.
	 * @return if this field is public; otherwise, .
	 */
	var IsPublic(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current field is security-critical or
	 * security-safe-critical at the current trust level.
	 * @return if the current field is security-critical or security-safe-critical at
	 * the current trust level;  if it is transparent.
	 */
	var IsSecurityCritical(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current field is security-safe-critical
	 * at the current trust level.
	 * @return if the current field is security-safe-critical at the current trust
	 * level;  if it is security-critical or transparent.
	 */
	var IsSecuritySafeCritical(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current field is transparent at the
	 * current trust level.
	 * @return if the field is security-transparent at the current trust level;
	 * otherwise, .
	 */
	var IsSecurityTransparent(default, never):Bool;
	/**
	 * Gets a value indicating whether the corresponding  attribute is set in the 
	 * enumerator.
	 * @return if the  attribute is set in ; otherwise, .
	 */
	var IsSpecialName(default, never):Bool;
	/**
	 * Gets a value indicating whether the field is static.
	 * @return if this field is static; otherwise, .
	 */
	var IsStatic(default, never):Bool;
	@:overload(function(handle:cs.system.RuntimeFieldHandle):cs.system.reflection.FieldInfo {})
	/**
	 * Gets a  for the field represented by the specified handle.
	 * @param handle A  structure that contains the handle to the internal metadata
	 * representation of a field.
	 * @return A  object representing the field specified by .
	 */
	static function GetFieldFromHandle(handle:cs.system.RuntimeFieldHandle, declaringType:cs.system.RuntimeTypeHandle):cs.system.reflection.FieldInfo;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.FieldInfo, right:cs.system.reflection.FieldInfo):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.FieldInfo, right:cs.system.reflection.FieldInfo):Bool;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Gets an array of types that identify the optional custom modifiers of the field.
	 * @return An array of  objects that identify the optional custom modifiers of the
	 * current field, such as .
	 */
	function GetOptionalCustomModifiers():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns a literal value associated with the field by a compiler.
	 * @return An  that contains the literal value associated with the field. If the
	 * literal value is a class type with an element value of zero, the return value is
	 * .
	 */
	function GetRawConstantValue():Dynamic;
	/**
	 * Gets an array of types that identify the required custom modifiers of the
	 * property.
	 * @return An array of  objects that identify the required custom modifiers of the
	 * current property, such as  or .
	 */
	function GetRequiredCustomModifiers():cs.NativeArray<cs.system.Type>;
	/**
	 * When overridden in a derived class, returns the value of a field supported by a
	 * given object.
	 * @param obj The object whose field value will be returned.
	 * @return An object containing the value of the field reflected by this instance.
	 */
	function GetValue(obj:Dynamic):Dynamic;
	/**
	 * Returns the value of a field supported by a given object.
	 * @param obj A  structure that encapsulates a managed pointer to a location and a
	 * runtime representation of the type that might be stored at that location.
	 * @return An  containing a field value.
	 */
	function GetValueDirect(obj:cs.system.TypedReference):Dynamic;
	@:overload(function(obj:Dynamic, value:Dynamic):Void {})
	/**
	 * Sets the value of the field supported by the given object.
	 * @param obj The object whose field value will be set.
	 * @param value The value to assign to the field.
	 */
	function SetValue(obj:Dynamic, value:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, culture:cs.system.globalization.CultureInfo):Void;
	/**
	 * Sets the value of the field supported by the given object.
	 * @param obj A  structure that encapsulates a managed pointer to a location and a
	 * runtime representation of the type that can be stored at that location.
	 * @param value The value to assign to the field.
	 */
	function SetValueDirect(obj:cs.system.TypedReference, value:Dynamic):Void;
}
