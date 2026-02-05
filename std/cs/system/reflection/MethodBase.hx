package cs.system.reflection;

/** Provides information about methods and constructors. */
@:native("System.Reflection.MethodBase")
extern class MethodBase extends cs.system.reflection.MemberInfo {
	/**
	 * Gets the attributes associated with this method.
	 * @return One of the  values.
	 */
	var Attributes(default, never):cs.system.reflection.MethodAttributes;
	/**
	 * Gets a value indicating the calling conventions for this method.
	 * @return The  for this method.
	 */
	var CallingConvention(default, never):cs.system.reflection.CallingConventions;
	/**
	 * Gets a value indicating whether the generic method contains unassigned generic
	 * type parameters.
	 * @return if the current  object represents a generic method that contains
	 * unassigned generic type parameters; otherwise, .
	 */
	var ContainsGenericParameters(default, never):Bool;
	/**
	 * Gets a value indicating whether the method is abstract.
	 * @return if the method is abstract; otherwise, .
	 */
	var IsAbstract(default, never):Bool;
	/**
	 * Gets a value indicating whether the potential visibility of this method or
	 * constructor is described by ; that is, the method or constructor is visible at
	 * most to other types in the same assembly, and is not visible to derived types
	 * outside the assembly.
	 * @return if the visibility of this method or constructor is exactly described by
	 * ; otherwise, .
	 */
	var IsAssembly(default, never):Bool;
	var IsConstructedGenericMethod(default, never):Bool;
	/**
	 * Gets a value indicating whether the method is a constructor.
	 * @return if this method is a constructor represented by a  object (see note in
	 * Remarks about  objects); otherwise, .
	 */
	var IsConstructor(default, never):Bool;
	/**
	 * Gets a value indicating whether the visibility of this method or constructor is
	 * described by ; that is, the method or constructor is visible only within its
	 * class and derived classes.
	 * @return if access to this method or constructor is exactly described by ;
	 * otherwise, .
	 */
	var IsFamily(default, never):Bool;
	/**
	 * Gets a value indicating whether the visibility of this method or constructor is
	 * described by ; that is, the method or constructor can be called by derived
	 * classes, but only if they are in the same assembly.
	 * @return if access to this method or constructor is exactly described by ;
	 * otherwise, .
	 */
	var IsFamilyAndAssembly(default, never):Bool;
	/**
	 * Gets a value indicating whether the potential visibility of this method or
	 * constructor is described by ; that is, the method or constructor can be called
	 * by derived classes wherever they are, and by classes in the same assembly.
	 * @return if access to this method or constructor is exactly described by ;
	 * otherwise, .
	 */
	var IsFamilyOrAssembly(default, never):Bool;
	/**
	 * Gets a value indicating whether this method is .
	 * @return if this method is ; otherwise, .
	 */
	var IsFinal(default, never):Bool;
	/**
	 * Gets a value indicating whether the method is generic.
	 * @return if the current  represents a generic method; otherwise, .
	 */
	var IsGenericMethod(default, never):Bool;
	/**
	 * Gets a value indicating whether the method is a generic method definition.
	 * @return if the current  object represents the definition of a generic method;
	 * otherwise, .
	 */
	var IsGenericMethodDefinition(default, never):Bool;
	/**
	 * Gets a value indicating whether only a member of the same kind with exactly the
	 * same signature is hidden in the derived class.
	 * @return if the member is hidden by signature; otherwise, .
	 */
	var IsHideBySig(default, never):Bool;
	/**
	 * Gets a value indicating whether this member is private.
	 * @return if access to this method is restricted to other members of the class
	 * itself; otherwise, .
	 */
	var IsPrivate(default, never):Bool;
	/**
	 * Gets a value indicating whether this is a public method.
	 * @return if this method is public; otherwise, .
	 */
	var IsPublic(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current method or constructor is
	 * security-critical or security-safe-critical at the current trust level, and
	 * therefore can perform critical operations.
	 * @return if the current method or constructor is security-critical or
	 * security-safe-critical at the current trust level;  if it is transparent.
	 */
	var IsSecurityCritical(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current method or constructor is
	 * security-safe-critical at the current trust level; that is, whether it can
	 * perform critical operations and can be accessed by transparent code.
	 * @return if the method or constructor is security-safe-critical at the current
	 * trust level;  if it is security-critical or transparent.
	 */
	var IsSecuritySafeCritical(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current method or constructor is
	 * transparent at the current trust level, and therefore cannot perform critical
	 * operations.
	 * @return if the method or constructor is security-transparent at the current
	 * trust level; otherwise, .
	 */
	var IsSecurityTransparent(default, never):Bool;
	/**
	 * Gets a value indicating whether this method has a special name.
	 * @return if this method has a special name; otherwise, .
	 */
	var IsSpecialName(default, never):Bool;
	/**
	 * Gets a value indicating whether the method is .
	 * @return if this method is ; otherwise, .
	 */
	var IsStatic(default, never):Bool;
	/**
	 * Gets a value indicating whether the method is .
	 * @return if this method is ; otherwise, .
	 */
	var IsVirtual(default, never):Bool;
	/**
	 * Gets a handle to the internal metadata representation of a method.
	 * @return A  object.
	 */
	var MethodHandle(default, never):cs.system.RuntimeMethodHandle;
	/**
	 * Gets the  flags that specify the attributes of a method implementation.
	 * @return The method implementation flags.
	 */
	var MethodImplementationFlags(default, never):cs.system.reflection.MethodImplAttributes;
	/**
	 * Returns a  object representing the currently executing method.
	 * @return is a static method that is called from within an executing method and
	 * that returns information about that method. A  object representing the currently
	 * executing method.
	 */
	static function GetCurrentMethod():cs.system.reflection.MethodBase;
	@:overload(function(handle:cs.system.RuntimeMethodHandle):cs.system.reflection.MethodBase {})
	/**
	 * Gets method information by using the method's internal metadata representation
	 * (handle).
	 * @param handle The method's handle.
	 * @return A  containing information about the method.
	 */
	static function GetMethodFromHandle(handle:cs.system.RuntimeMethodHandle, declaringType:cs.system.RuntimeTypeHandle):cs.system.reflection.MethodBase;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.MethodBase, right:cs.system.reflection.MethodBase):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.MethodBase, right:cs.system.reflection.MethodBase):Bool;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns an array of  objects that represent the type arguments of a generic
	 * method or the type parameters of a generic method definition.
	 * @return An array of  objects that represent the type arguments of a generic
	 * method or the type parameters of a generic method definition. Returns an empty
	 * array if the current method is not a generic method.
	 */
	function GetGenericArguments():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * When overridden in a derived class, gets a  object that provides access to the
	 * MSIL stream, local variables, and exceptions for the current method.
	 * @return A  object that provides access to the MSIL stream, local variables, and
	 * exceptions for the current method.
	 */
	function GetMethodBody():cs.system.reflection.MethodBody;
	/**
	 * When overridden in a derived class, returns the  flags.
	 * @return The  flags.
	 */
	function GetMethodImplementationFlags():cs.system.reflection.MethodImplAttributes;
	/**
	 * When overridden in a derived class, gets the parameters of the specified method
	 * or constructor.
	 * @return An array of type  containing information that matches the signature of
	 * the method (or constructor) reflected by this  instance.
	 */
	function GetParameters():cs.NativeArray<cs.system.reflection.ParameterInfo>;
	@:overload(function(obj:Dynamic, parameters:cs.NativeArray<Dynamic>):Dynamic {})
	/**
	 * Invokes the method or constructor represented by the current instance, using the
	 * specified parameters.
	 * @param obj The object on which to invoke the method or constructor. If a method
	 * is static, this argument is ignored. If a constructor is static, this argument
	 * must be  or an instance of the class that defines the constructor.
	 * @param parameters An argument list for the invoked method or constructor. This
	 * is an array of objects with the same number, order, and type as the parameters
	 * of the method or constructor to be invoked. If there are no parameters,  should
	 * be . If the method or constructor represented by this instance takes a 
	 * parameter ( in Visual Basic), no special attribute is required for that
	 * parameter in order to invoke the method or constructor using this function. Any
	 * object in this array that is not explicitly initialized with a value will
	 * contain the default value for that object type. For reference-type elements,
	 * this value is . For value-type elements, this value is 0, 0.0, or , depending on
	 * the specific element type.
	 * @return An object containing the return value of the invoked method, or  in the
	 * case of a constructor.
	 */
	function Invoke(obj:Dynamic, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, parameters:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic;
}
