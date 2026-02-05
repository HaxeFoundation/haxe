package cs.system.reflection;

/** Discovers the attributes of a method and provides access to method metadata. */
@:native("System.Reflection.MethodInfo")
extern class MethodInfo extends cs.system.reflection.MethodBase {
	/**
	 * Gets a  object that contains information about the return type of the method,
	 * such as whether the return type has custom modifiers.
	 * @return A  object that contains information about the return type.
	 */
	var ReturnParameter(default, never):cs.system.reflection.ParameterInfo;
	/**
	 * Gets the return type of this method.
	 * @return The return type of this method.
	 */
	var ReturnType(default, never):cs.system.Type;
	/**
	 * Gets the custom attributes for the return type.
	 * @return An  object representing the custom attributes for the return type.
	 */
	var ReturnTypeCustomAttributes(default, never):cs.system.reflection.ICustomAttributeProvider;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.MethodInfo, right:cs.system.reflection.MethodInfo):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.MethodInfo, right:cs.system.reflection.MethodInfo):Bool;
	@:overload(function(delegateType:cs.system.Type):cs.system.Delegate {})
	/**
	 * Creates a delegate of the specified type from this method.
	 * @param delegateType The type of the delegate to create.
	 * @return The delegate for this method.
	 */
	function CreateDelegate(delegateType:cs.system.Type, target:Dynamic):cs.system.Delegate;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * When overridden in a derived class, returns the  object for the method on the
	 * direct or indirect base class in which the method represented by this instance
	 * was first declared.
	 * @return A  object for the first implementation of this method.
	 */
	function GetBaseDefinition():cs.system.reflection.MethodInfo;
	/**
	 * Returns an array of  objects that represent the type arguments of a generic
	 * method or the type parameters of a generic method definition.
	 * @return An array of  objects that represent the type arguments of a generic
	 * method or the type parameters of a generic method definition. Returns an empty
	 * array if the current method is not a generic method.
	 */
	function GetGenericArguments():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns a  object that represents a generic method definition from which the
	 * current method can be constructed.
	 * @return A  object representing a generic method definition from which the
	 * current method can be constructed.
	 */
	function GetGenericMethodDefinition():cs.system.reflection.MethodInfo;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Substitutes the elements of an array of types for the type parameters of the
	 * current generic method definition, and returns a  object representing the
	 * resulting constructed method.
	 * @param typeArguments An array of types to be substituted for the type parameters
	 * of the current generic method definition.
	 * @return A  object that represents the constructed method formed by substituting
	 * the elements of  for the type parameters of the current generic method
	 * definition.
	 */
	function MakeGenericMethod(typeArguments:cs.NativeArray<cs.system.Type>):cs.system.reflection.MethodInfo;
}
