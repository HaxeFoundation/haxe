package cs.system;

/** Represents type declarations: class types, interface types, array types, value types, enumeration types, type parameters, generic type definitions, and open or closed constructed generic types. */
@:native("System.Type")
extern class Type extends cs.system.reflection.MemberInfo {
	/** Separates names in the namespace of the . This field is read-only. */
	static var Delimiter(default, never):cs.Char16;
	/** Represents an empty array of type . This field is read-only. */
	static var EmptyTypes(default, never):cs.NativeArray<cs.system.Type>;
	/** Represents the member filter used on attributes. This field is read-only. */
	static var FilterAttribute(default, never):cs.system.reflection.MemberFilter;
	/** Represents the case-sensitive member filter used on names. This field is read-only. */
	static var FilterName(default, never):cs.system.reflection.MemberFilter;
	/** Represents the case-insensitive member filter used on names. This field is read-only. */
	static var FilterNameIgnoreCase(default, never):cs.system.reflection.MemberFilter;
	/** Represents a missing value in the  information. This field is read-only. */
	static var Missing(default, never):Dynamic;
	/**
	 * Gets a reference to the default binder, which implements internal rules for
	 * selecting the appropriate members to be called by .
	 * @return A reference to the default binder used by the system.
	 */
	static var DefaultBinder(default, never):cs.system.reflection.Binder;
	/**
	 * Gets the  in which the type is declared. For generic types, gets the  in which
	 * the generic type is defined.
	 * @return An  instance that describes the assembly containing the current type.
	 * For generic types, the instance describes the assembly that contains the generic
	 * type definition, not the assembly that creates and uses a particular constructed
	 * type.
	 */
	var Assembly(default, never):cs.system.reflection.Assembly;
	/**
	 * Gets the assembly-qualified name of the type, which includes the name of the
	 * assembly from which this  object was loaded.
	 * @return The assembly-qualified name of the , which includes the name of the
	 * assembly from which the  was loaded, or  if the current instance represents a
	 * generic type parameter.
	 */
	var AssemblyQualifiedName(default, never):String;
	/**
	 * Gets the attributes associated with the .
	 * @return A  object representing the attribute set of the , unless the  represents
	 * a generic type parameter, in which case the value is unspecified.
	 */
	var Attributes(default, never):cs.system.reflection.TypeAttributes;
	/**
	 * Gets the type from which the current  directly inherits.
	 * @return The  from which the current  directly inherits, or  if the current 
	 * represents the  class or an interface.
	 */
	var BaseType(default, never):cs.system.Type;
	/**
	 * Gets a value indicating whether the current  object has type parameters that
	 * have not been replaced by specific types.
	 * @return if the  object is itself a generic type parameter or has type parameters
	 * for which specific types have not been supplied; otherwise, .
	 */
	var ContainsGenericParameters(default, never):Bool;
	/**
	 * Gets a  that represents the declaring method, if the current  represents a type
	 * parameter of a generic method.
	 * @return If the current  represents a type parameter of a generic method, a  that
	 * represents declaring method; otherwise, .
	 */
	var DeclaringMethod(default, never):cs.system.reflection.MethodBase;
	/**
	 * Gets the fully qualified name of the type, including its namespace but not its
	 * assembly.
	 * @return The fully qualified name of the type, including its namespace but not
	 * its assembly; or  if the current instance represents a generic type parameter,
	 * an array type, pointer type, or  type based on a type parameter, or a generic
	 * type that is not a generic type definition but contains unresolved type
	 * parameters.
	 */
	var FullName(default, never):String;
	/**
	 * Gets a combination of  flags that describe the covariance and special
	 * constraints of the current generic type parameter.
	 * @return A bitwise combination of  values that describes the covariance and
	 * special constraints of the current generic type parameter.
	 */
	var GenericParameterAttributes(default, never):cs.system.reflection.GenericParameterAttributes;
	/**
	 * Gets the position of the type parameter in the type parameter list of the
	 * generic type or method that declared the parameter, when the  object represents
	 * a type parameter of a generic type or a generic method.
	 * @return The position of a type parameter in the type parameter list of the
	 * generic type or method that defines the parameter. Position numbers begin at 0.
	 */
	var GenericParameterPosition(default, never):Int;
	/**
	 * Gets an array of the generic type arguments for this type.
	 * @return An array of the generic type arguments for this type.
	 */
	var GenericTypeArguments(default, never):cs.NativeArray<cs.system.Type>;
	/**
	 * Gets the GUID associated with the .
	 * @return The GUID associated with the .
	 */
	var GUID(default, never):cs.system.Guid;
	/**
	 * Gets a value indicating whether the current  encompasses or refers to another
	 * type; that is, whether the current  is an array, a pointer, or is passed by
	 * reference.
	 * @return if the  is an array, a pointer, or is passed by reference; otherwise, .
	 */
	var HasElementType(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is abstract and must be overridden.
	 * @return if the  is abstract; otherwise, .
	 */
	var IsAbstract(default, never):Bool;
	/**
	 * Gets a value indicating whether the string format attribute  is selected for the
	 * .
	 * @return if the string format attribute  is selected for the ; otherwise, .
	 */
	var IsAnsiClass(default, never):Bool;
	/**
	 * Gets a value that indicates whether the type is an array.
	 * @return if the current type is an array; otherwise, .
	 */
	var IsArray(default, never):Bool;
	/**
	 * Gets a value indicating whether the string format attribute  is selected for the
	 * .
	 * @return if the string format attribute  is selected for the ; otherwise, .
	 */
	var IsAutoClass(default, never):Bool;
	/**
	 * Gets a value indicating whether the fields of the current type are laid out
	 * automatically by the common language runtime.
	 * @return if the  property of the current type includes ; otherwise, .
	 */
	var IsAutoLayout(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is passed by reference.
	 * @return if the  is passed by reference; otherwise, .
	 */
	var IsByRef(default, never):Bool;
	var IsByRefLike(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is a class or a delegate; that is, not a
	 * value type or interface.
	 * @return if the  is a class; otherwise, .
	 */
	var IsClass(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is a COM object.
	 * @return if the  is a COM object; otherwise, .
	 */
	var IsCOMObject(default, never):Bool;
	/**
	 * Gets a value that indicates whether this object represents a constructed generic
	 * type. You can create instances of a constructed generic type.
	 * @return if this object represents a constructed generic type; otherwise, .
	 */
	var IsConstructedGenericType(default, never):Bool;
	/**
	 * Gets a value indicating whether the  can be hosted in a context.
	 * @return if the  can be hosted in a context; otherwise, .
	 */
	var IsContextful(default, never):Bool;
	/**
	 * Gets a value indicating whether the current  represents an enumeration.
	 * @return if the current  represents an enumeration; otherwise, .
	 */
	var IsEnum(default, never):Bool;
	/**
	 * Gets a value indicating whether the fields of the current type are laid out at
	 * explicitly specified offsets.
	 * @return if the  property of the current type includes ; otherwise, .
	 */
	var IsExplicitLayout(default, never):Bool;
	var IsGenericMethodParameter(default, never):Bool;
	/**
	 * Gets a value indicating whether the current  represents a type parameter in the
	 * definition of a generic type or method.
	 * @return if the  object represents a type parameter of a generic type definition
	 * or generic method definition; otherwise, .
	 */
	var IsGenericParameter(default, never):Bool;
	/**
	 * Gets a value indicating whether the current type is a generic type.
	 * @return if the current type is a generic type; otherwise, .
	 */
	var IsGenericType(default, never):Bool;
	/**
	 * Gets a value indicating whether the current  represents a generic type
	 * definition, from which other generic types can be constructed.
	 * @return if the  object represents a generic type definition; otherwise, .
	 */
	var IsGenericTypeDefinition(default, never):Bool;
	var IsGenericTypeParameter(default, never):Bool;
	/**
	 * Gets a value indicating whether the  has a  attribute applied, indicating that
	 * it was imported from a COM type library.
	 * @return if the  has a ; otherwise, .
	 */
	var IsImport(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is an interface; that is, not a class or a
	 * value type.
	 * @return if the  is an interface; otherwise, .
	 */
	var IsInterface(default, never):Bool;
	/**
	 * Gets a value indicating whether the fields of the current type are laid out
	 * sequentially, in the order that they were defined or emitted to the metadata.
	 * @return if the  property of the current type includes ; otherwise, .
	 */
	var IsLayoutSequential(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is marshaled by reference.
	 * @return if the  is marshaled by reference; otherwise, .
	 */
	var IsMarshalByRef(default, never):Bool;
	/**
	 * Gets a value indicating whether the current  object represents a type whose
	 * definition is nested inside the definition of another type.
	 * @return if the  is nested inside another type; otherwise, .
	 */
	var IsNested(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is nested and visible only within its own
	 * assembly.
	 * @return if the  is nested and visible only within its own assembly; otherwise, .
	 */
	var IsNestedAssembly(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is nested and visible only to classes that
	 * belong to both its own family and its own assembly.
	 * @return if the  is nested and visible only to classes that belong to both its
	 * own family and its own assembly; otherwise, .
	 */
	var IsNestedFamANDAssem(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is nested and visible only within its own
	 * family.
	 * @return if the  is nested and visible only within its own family; otherwise, .
	 */
	var IsNestedFamily(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is nested and visible only to classes that
	 * belong to either its own family or to its own assembly.
	 * @return if the  is nested and visible only to classes that belong to its own
	 * family or to its own assembly; otherwise, .
	 */
	var IsNestedFamORAssem(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is nested and declared private.
	 * @return if the  is nested and declared private; otherwise, .
	 */
	var IsNestedPrivate(default, never):Bool;
	/**
	 * Gets a value indicating whether a class is nested and declared public.
	 * @return if the class is nested and declared public; otherwise, .
	 */
	var IsNestedPublic(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is not declared public.
	 * @return if the  is not declared public and is not a nested type; otherwise, .
	 */
	var IsNotPublic(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is a pointer.
	 * @return if the  is a pointer; otherwise, .
	 */
	var IsPointer(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is one of the primitive types.
	 * @return if the  is one of the primitive types; otherwise, .
	 */
	var IsPrimitive(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is declared public.
	 * @return if the  is declared public and is not a nested type; otherwise, .
	 */
	var IsPublic(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is declared sealed.
	 * @return if the  is declared sealed; otherwise, .
	 */
	var IsSealed(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current type is security-critical or
	 * security-safe-critical at the current trust level, and therefore can perform
	 * critical operations.
	 * @return if the current type is security-critical or security-safe-critical at
	 * the current trust level;  if it is transparent.
	 */
	var IsSecurityCritical(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current type is security-safe-critical
	 * at the current trust level; that is, whether it can perform critical operations
	 * and can be accessed by transparent code.
	 * @return if the current type is security-safe-critical at the current trust
	 * level;  if it is security-critical or transparent.
	 */
	var IsSecuritySafeCritical(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current type is transparent at the
	 * current trust level, and therefore cannot perform critical operations.
	 * @return if the type is security-transparent at the current trust level;
	 * otherwise, .
	 */
	var IsSecurityTransparent(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is serializable.
	 * @return if the  is serializable; otherwise, .
	 */
	var IsSerializable(default, never):Bool;
	var IsSignatureType(default, never):Bool;
	/**
	 * Gets a value indicating whether the type has a name that requires special
	 * handling.
	 * @return if the type has a name that requires special handling; otherwise, .
	 */
	var IsSpecialName(default, never):Bool;
	var IsSZArray(default, never):Bool;
	var IsTypeDefinition(default, never):Bool;
	/**
	 * Gets a value indicating whether the string format attribute  is selected for the
	 * .
	 * @return if the string format attribute  is selected for the ; otherwise, .
	 */
	var IsUnicodeClass(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is a value type.
	 * @return if the  is a value type; otherwise, .
	 */
	var IsValueType(default, never):Bool;
	var IsVariableBoundArray(default, never):Bool;
	/**
	 * Gets a value indicating whether the  can be accessed by code outside the
	 * assembly.
	 * @return if the current  is a public type or a public nested type such that all
	 * the enclosing types are public; otherwise, .
	 */
	var IsVisible(default, never):Bool;
	/**
	 * Gets the namespace of the .
	 * @return The namespace of the ;  if the current instance has no namespace or
	 * represents a generic parameter.
	 */
	var Namespace(default, never):String;
	/**
	 * Gets a  that describes the layout of the current type.
	 * @return Gets a  that describes the gross layout features of the current type.
	 */
	var StructLayoutAttribute(default, never):cs.system.runtime.interopservices.StructLayoutAttribute;
	/**
	 * Gets the handle for the current .
	 * @return The handle for the current .
	 */
	var TypeHandle(default, never):cs.system.RuntimeTypeHandle;
	/**
	 * Gets the initializer for the type.
	 * @return An object that contains the name of the class constructor for the .
	 */
	var TypeInitializer(default, never):cs.system.reflection.ConstructorInfo;
	/**
	 * Indicates the type provided by the common language runtime that represents this
	 * type.
	 * @return The underlying system type for the .
	 */
	var UnderlyingSystemType(default, never):cs.system.Type;
	@:overload(function(typeName:String):cs.system.Type {})
	@:overload(function(typeName:String, throwOnError:Bool):cs.system.Type {})
	@:overload(function(typeName:String, throwOnError:Bool, ignoreCase:Bool):cs.system.Type {})
	@:overload(function(typeName:String, assemblyResolver:cs.system.Func_2<cs.system.reflection.AssemblyName, cs.system.reflection.Assembly>, typeResolver:cs.system.Func_4<cs.system.reflection.Assembly, String, Bool, cs.system.Type>):cs.system.Type {})
	@:overload(function(typeName:String, assemblyResolver:cs.system.Func_2<cs.system.reflection.AssemblyName, cs.system.reflection.Assembly>, typeResolver:cs.system.Func_4<cs.system.reflection.Assembly, String, Bool, cs.system.Type>, throwOnError:Bool):cs.system.Type {})
	/**
	 * Gets the current .
	 * @return The current .
	 */
	static function GetType(typeName:String, assemblyResolver:cs.system.Func_2<cs.system.reflection.AssemblyName, cs.system.reflection.Assembly>, typeResolver:cs.system.Func_4<cs.system.reflection.Assembly, String, Bool, cs.system.Type>, throwOnError:Bool, ignoreCase:Bool):cs.system.Type;
	/**
	 * Gets the types of the objects in the specified array.
	 * @param args An array of objects whose types to determine.
	 * @return An array of  objects representing the types of the corresponding
	 * elements in .
	 */
	static function GetTypeArray(args:cs.NativeArray<Dynamic>):cs.NativeArray<cs.system.Type>;
	/**
	 * Gets the underlying type code of the specified .
	 * @param type The type whose underlying type code to get.
	 * @return The code of the underlying type, or  if  is .
	 */
	static function GetTypeCode(type:cs.system.Type):cs.system.TypeCode;
	@:overload(function(clsid:cs.system.Guid):cs.system.Type {})
	@:overload(function(clsid:cs.system.Guid, throwOnError:Bool):cs.system.Type {})
	@:overload(function(clsid:cs.system.Guid, server:String):cs.system.Type {})
	/**
	 * Gets the type associated with the specified class identifier (CLSID).
	 * @param clsid The CLSID of the type to get.
	 * @return regardless of whether the CLSID is valid.
	 */
	static function GetTypeFromCLSID(clsid:cs.system.Guid, server:String, throwOnError:Bool):cs.system.Type;
	/**
	 * Gets the type referenced by the specified type handle.
	 * @param handle The object that refers to the type.
	 * @return The type referenced by the specified , or  if the  property of  is .
	 */
	static function GetTypeFromHandle(handle:cs.system.RuntimeTypeHandle):cs.system.Type;
	@:overload(function(progID:String):cs.system.Type {})
	@:overload(function(progID:String, throwOnError:Bool):cs.system.Type {})
	@:overload(function(progID:String, server:String):cs.system.Type {})
	/**
	 * Gets the type associated with the specified program identifier (ProgID),
	 * returning null if an error is encountered while loading the .
	 * @param progID The ProgID of the type to get.
	 * @return The type associated with the specified ProgID, if  is a valid entry in
	 * the registry and a type is associated with it; otherwise, .
	 */
	static function GetTypeFromProgID(progID:String, server:String, throwOnError:Bool):cs.system.Type;
	/**
	 * Gets the handle for the  of a specified object.
	 * @param o The object for which to get the type handle.
	 * @return The handle for the  of the specified .
	 */
	static function GetTypeHandle(o:Dynamic):cs.system.RuntimeTypeHandle;
	/** @param position  */
	static function MakeGenericMethodParameter(position:Int):cs.system.Type;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.Type, right:cs.system.Type):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.Type, right:cs.system.Type):Bool;
	/**
	 * Gets the  with the specified name, specifying whether to perform a
	 * case-sensitive search and whether to throw an exception if the type is not
	 * found. The type is loaded for reflection only, not for execution.
	 * @param typeName The assembly-qualified name of the  to get.
	 * @param throwIfNotFound to throw a  if the type cannot be found;  to return  if
	 * the type cannot be found. Specifying  also suppresses some other exception
	 * conditions, but not all of them. See the Exceptions section.
	 * @param ignoreCase to perform a case-insensitive search for ;  to perform a
	 * case-sensitive search for .
	 * @return The type with the specified name, if found; otherwise, . If the type is
	 * not found, the  parameter specifies whether  is returned or an exception is
	 * thrown. In some cases, an exception is thrown regardless of the value of . See
	 * the Exceptions section.
	 */
	static function ReflectionOnlyGetType(typeName:String, throwIfNotFound:Bool, ignoreCase:Bool):cs.system.Type;
	@:overload(function(o:Dynamic):Bool {})
	/**
	 * Determines if the underlying system type of the current  object is the same as
	 * the underlying system type of the specified .
	 * @param o The object whose underlying system type is to be compared with the
	 * underlying system type of the current . For the comparison to succeed,  must be
	 * able to be cast or converted to an object of type   .
	 * @return if the underlying system type of  is the same as the underlying system
	 * type of the current ; otherwise, . This method also returns  if: . is . cannot
	 * be cast or converted to a  object.
	 */
	function Equals(o:cs.system.Type):Bool;
	/**
	 * Returns an array of  objects representing a filtered list of interfaces
	 * implemented or inherited by the current .
	 * @param filter The delegate that compares the interfaces against .
	 * @param filterCriteria The search criteria that determines whether an interface
	 * should be included in the returned array.
	 * @return An array of  objects representing a filtered list of the interfaces
	 * implemented or inherited by the current , or an empty array if no interfaces
	 * matching the filter are implemented or inherited by the current .
	 */
	function FindInterfaces(filter:cs.system.reflection.TypeFilter, filterCriteria:Dynamic):cs.NativeArray<cs.system.Type>;
	/**
	 * Returns a filtered array of  objects of the specified member type.
	 * @param memberType A bitwise combination of the enumeration values that indicates
	 * the type of member to search for.
	 * @param bindingAttr A bitwise combination of the enumeration values that specify
	 * how the search is conducted. -or- to return .
	 * @param filter The delegate that does the comparisons, returning  if the member
	 * currently being inspected matches the  and  otherwise.
	 * @param filterCriteria The search criteria that determines whether a member is
	 * returned in the array of  objects. The fields of , , and  can be used in
	 * conjunction with the  delegate supplied by this class.
	 * @return A filtered array of  objects of the specified member type. -or- An empty
	 * array if the current  does not have members of type  that match the filter
	 * criteria.
	 */
	function FindMembers(memberType:cs.system.reflection.MemberTypes, bindingAttr:cs.system.reflection.BindingFlags, filter:cs.system.reflection.MemberFilter, filterCriteria:Dynamic):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Gets the number of dimensions in an array.
	 * @return An integer that contains the number of dimensions in the current type.
	 */
	function GetArrayRank():Int;
	@:overload(function(types:cs.NativeArray<cs.system.Type>):cs.system.reflection.ConstructorInfo {})
	@:overload(function(bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.ConstructorInfo {})
	/**
	 * Searches for a constructor whose parameters match the specified argument types
	 * and modifiers, using the specified binding constraints and the specified calling
	 * convention.
	 * @param bindingAttr A bitwise combination of the enumeration values that specify
	 * how the search is conducted. -or- to return .
	 * @param binder An object that defines a set of properties and enables binding,
	 * which can involve selection of an overloaded method, coercion of argument types,
	 * and invocation of a member through reflection. -or- A null reference ( in Visual
	 * Basic), to use the .
	 * @param callConvention The object that specifies the set of rules to use
	 * regarding the order and layout of arguments, how the return value is passed,
	 * what registers are used for arguments, and the stack is cleaned up.
	 * @param types An array of  objects representing the number, order, and type of
	 * the parameters for the constructor to get. -or- An empty array of the type 
	 * (that is, Type[] types = new Type[0]) to get a constructor that takes no
	 * parameters.
	 * @param modifiers An array of  objects representing the attributes associated
	 * with the corresponding element in the  array. The default binder does not
	 * process this parameter.
	 * @return An object representing the constructor that matches the specified
	 * requirements, if found; otherwise, .
	 */
	function GetConstructor(bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, callConvention:cs.system.reflection.CallingConventions, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.ConstructorInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.ConstructorInfo> {})
	/**
	 * Returns all the public constructors defined for the current .
	 * @return An array of  objects representing all the public instance constructors
	 * defined for the current , but not including the type initializer (static
	 * constructor). If no public instance constructors are defined for the current ,
	 * or if the current  represents a type parameter in the definition of a generic
	 * type or generic method, an empty array of type  is returned.
	 */
	function GetConstructors(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.ConstructorInfo>;
	/**
	 * Searches for the members defined for the current  whose  is set.
	 * @return An array of  objects representing all default members of the current .
	 * -or- An empty array of type , if the current  does not have default members.
	 */
	function GetDefaultMembers():cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * When overridden in a derived class, returns the  of the object encompassed or
	 * referred to by the current array, pointer or reference type.
	 * @return The  of the object encompassed or referred to by the current array,
	 * pointer, or reference type, or  if the current  is not an array or a pointer, or
	 * is not passed by reference, or represents a generic type or a type parameter in
	 * the definition of a generic type or generic method.
	 */
	function GetElementType():cs.system.Type;
	/**
	 * Returns the name of the constant that has the specified value, for the current
	 * enumeration type.
	 * @param value The value whose name is to be retrieved.
	 * @return The name of the member of the current enumeration type that has the
	 * specified value, or  if no such constant is found.
	 */
	function GetEnumName(value:Dynamic):String;
	/**
	 * Returns the names of the members of the current enumeration type.
	 * @return An array that contains the names of the members of the enumeration.
	 */
	function GetEnumNames():cs.NativeArray<String>;
	/**
	 * Returns the underlying type of the current enumeration type.
	 * @return The underlying type of the current enumeration.
	 */
	function GetEnumUnderlyingType():cs.system.Type;
	/**
	 * Returns an array of the values of the constants in the current enumeration type.
	 * @return An array that contains the values. The elements of the array are sorted
	 * by the binary values (that is, the unsigned values) of the enumeration
	 * constants.
	 */
	function GetEnumValues():cs.system.Array;
	@:overload(function(name:String):cs.system.reflection.EventInfo {})
	/**
	 * Returns the  object representing the specified public event.
	 * @param name The string containing the name of an event that is declared or
	 * inherited by the current .
	 * @return The object representing the specified public event that is declared or
	 * inherited by the current , if found; otherwise, .
	 */
	function GetEvent(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.EventInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.EventInfo> {})
	/**
	 * Returns all the public events that are declared or inherited by the current .
	 * @return An array of  objects representing all the public events which are
	 * declared or inherited by the current . -or- An empty array of type , if the
	 * current  does not have public events.
	 */
	function GetEvents(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.EventInfo>;
	@:overload(function(name:String):cs.system.reflection.FieldInfo {})
	/**
	 * Searches for the public field with the specified name.
	 * @param name The string containing the name of the data field to get.
	 * @return An object representing the public field with the specified name, if
	 * found; otherwise, .
	 */
	function GetField(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.FieldInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.FieldInfo> {})
	/**
	 * Returns all the public fields of the current .
	 * @return An array of  objects representing all the public fields defined for the
	 * current . -or- An empty array of type , if no public fields are defined for the
	 * current .
	 */
	function GetFields(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.FieldInfo>;
	/**
	 * Returns an array of  objects that represent the type arguments of a closed
	 * generic type or the type parameters of a generic type definition.
	 * @return An array of  objects that represent the type arguments of a generic
	 * type. Returns an empty array if the current type is not a generic type.
	 */
	function GetGenericArguments():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns an array of  objects that represent the constraints on the current
	 * generic type parameter.
	 * @return An array of  objects that represent the constraints on the current
	 * generic type parameter.
	 */
	function GetGenericParameterConstraints():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns a  object that represents a generic type definition from which the
	 * current generic type can be constructed.
	 * @return A  object representing a generic type from which the current type can be
	 * constructed.
	 */
	function GetGenericTypeDefinition():cs.system.Type;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code for this instance.
	 */
	function GetHashCode():Int;
	@:overload(function(name:String):cs.system.Type {})
	/**
	 * Searches for the interface with the specified name.
	 * @param name The string containing the name of the interface to get. For generic
	 * interfaces, this is the mangled name.
	 * @return An object representing the interface with the specified name,
	 * implemented or inherited by the current , if found; otherwise, .
	 */
	function GetInterface(name:String, ignoreCase:Bool):cs.system.Type;
	/**
	 * Returns an interface mapping for the specified interface type.
	 * @param interfaceType The interface type to retrieve a mapping for.
	 * @return An object that represents the interface mapping for .
	 */
	function GetInterfaceMap(interfaceType:cs.system.Type):cs.system.reflection.InterfaceMapping;
	/**
	 * When overridden in a derived class, gets all the interfaces implemented or
	 * inherited by the current .
	 * @return An array of  objects representing all the interfaces implemented or
	 * inherited by the current . -or- An empty array of type , if no interfaces are
	 * implemented or inherited by the current .
	 */
	function GetInterfaces():cs.NativeArray<cs.system.Type>;
	@:overload(function(name:String):cs.NativeArray<cs.system.reflection.MemberInfo> {})
	@:overload(function(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo> {})
	/**
	 * Searches for the public members with the specified name.
	 * @param name The string containing the name of the public members to get.
	 * @return An array of  objects representing the public members with the specified
	 * name, if found; otherwise, an empty array.
	 */
	function GetMember(name:String, type:cs.system.reflection.MemberTypes, bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	@:overload(function():cs.NativeArray<cs.system.reflection.MemberInfo> {})
	/**
	 * Returns all the public members of the current .
	 * @return An array of  objects representing all the public members of the current
	 * . -or- An empty array of type , if the current  does not have public members.
	 */
	function GetMembers(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	@:overload(function(name:String):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, types:cs.NativeArray<cs.system.Type>):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, genericParameterCount:Int, types:cs.NativeArray<cs.system.Type>):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, genericParameterCount:Int, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, genericParameterCount:Int, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodInfo {})
	@:overload(function(name:String, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, callConvention:cs.system.reflection.CallingConventions, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodInfo {})
	/**
	 * Searches for the public method with the specified name.
	 * @param name The string containing the name of the public method to get.
	 * @return An object that represents the public method with the specified name, if
	 * found; otherwise, .
	 */
	function GetMethod(name:String, genericParameterCount:Int, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, callConvention:cs.system.reflection.CallingConventions, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.MethodInfo> {})
	/**
	 * Returns all the public methods of the current .
	 * @return An array of  objects representing all the public methods defined for the
	 * current . -or- An empty array of type , if no public methods are defined for the
	 * current .
	 */
	function GetMethods(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MethodInfo>;
	@:overload(function(name:String):cs.system.Type {})
	/**
	 * Searches for the public nested type with the specified name.
	 * @param name The string containing the name of the nested type to get.
	 * @return An object representing the public nested type with the specified name,
	 * if found; otherwise, .
	 */
	function GetNestedType(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.Type;
	@:overload(function():cs.NativeArray<cs.system.Type> {})
	/**
	 * Returns the public types nested in the current .
	 * @return An array of  objects representing the public types nested in the current
	 * (the search is not recursive), or an empty array of type  if no public types are
	 * nested in the current .
	 */
	function GetNestedTypes(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.Type>;
	@:overload(function():cs.NativeArray<cs.system.reflection.PropertyInfo> {})
	/**
	 * Returns all the public properties of the current .
	 * @return An array of  objects representing all public properties of the current .
	 * -or- An empty array of type , if the current  does not have public properties.
	 */
	function GetProperties(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.PropertyInfo>;
	@:overload(function(name:String):cs.system.reflection.PropertyInfo {})
	@:overload(function(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.PropertyInfo {})
	@:overload(function(name:String, returnType:cs.system.Type):cs.system.reflection.PropertyInfo {})
	@:overload(function(name:String, types:cs.NativeArray<cs.system.Type>):cs.system.reflection.PropertyInfo {})
	@:overload(function(name:String, returnType:cs.system.Type, types:cs.NativeArray<cs.system.Type>):cs.system.reflection.PropertyInfo {})
	@:overload(function(name:String, returnType:cs.system.Type, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.PropertyInfo {})
	/**
	 * Searches for the public property with the specified name.
	 * @param name The string containing the name of the public property to get.
	 * @return An object representing the public property with the specified name, if
	 * found; otherwise, .
	 */
	function GetProperty(name:String, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, returnType:cs.system.Type, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.PropertyInfo;
	/**
	 * Gets the current .
	 * @return The current .
	 */
	function GetType():cs.system.Type;
	@:overload(function(name:String, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, target:Dynamic, args:cs.NativeArray<Dynamic>):Dynamic {})
	@:overload(function(name:String, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, target:Dynamic, args:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic {})
	/**
	 * Invokes the specified member, using the specified binding constraints and
	 * matching the specified argument list.
	 * @param name The string containing the name of the constructor, method, property,
	 * or field member to invoke. -or- An empty string ("") to invoke the default
	 * member. -or- For  members, a string representing the DispID, for example
	 * "[DispID=3]".
	 * @param invokeAttr A bitwise combination of the enumeration values that specify
	 * how the search is conducted. The access can be one of the  such as , , , , , and
	 * so on. The type of lookup need not be specified. If the type of lookup is
	 * omitted,  |  |  are used.
	 * @param binder An object that defines a set of properties and enables binding,
	 * which can involve selection of an overloaded method, coercion of argument types,
	 * and invocation of a member through reflection. -or- A null reference ( in Visual
	 * Basic), to use the . Note that explicitly defining a  object may be required for
	 * successfully invoking method overloads with variable arguments.
	 * @param target The object on which to invoke the specified member.
	 * @param args An array containing the arguments to pass to the member to invoke.
	 * @return An object representing the return value of the invoked member.
	 */
	function InvokeMember(name:String, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, target:Dynamic, args:cs.NativeArray<Dynamic>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>, culture:cs.system.globalization.CultureInfo, namedParameters:cs.NativeArray<String>):Dynamic;
	/**
	 * Determines whether an instance of a specified type can be assigned to a variable
	 * of the current type.
	 * @param c The type to compare with the current type.
	 * @return if any of the following conditions is true: and the current instance
	 * represent the same type. is derived either directly or indirectly from the
	 * current instance.  is derived directly from the current instance if it inherits
	 * from the current instance;  is derived indirectly from the current instance if
	 * it inherits from a succession of one or more classes that inherit from the
	 * current instance. The current instance is an interface that  implements. is a
	 * generic type parameter, and the current instance represents one of the
	 * constraints of . In the following example, the current instance is a  object
	 * that represents the  class. GenericWithConstraint is a generic type whose
	 * generic type parameter must be of type    . Passing its generic type parameter
	 * to the  indicates that  an instance of the generic type parameter can be
	 * assigned to an  object. using System; using System.IO; public class Example {
	 * public static void Main() { Type t = typeof(Stream); Type genericT =
	 * typeof(GenericWithConstraint<>); Type genericParam =
	 * genericT.GetGenericArguments()[0];
	 * Console.WriteLine(t.IsAssignableFrom(genericParam)); // Displays True. } }
	 * public class GenericWithConstraint<T> where T : Stream {} Imports System.IO
	 * Module Example Public Sub Main() Dim t As Type = GetType(Stream) Dim genericT As
	 * Type = GetType(GenericWithConstraint(Of )) Dim genericParam As Type =
	 * genericT.GetGenericArguments()(0)
	 * Console.WriteLine(t.IsAssignableFrom(genericParam)) ' Displays True. End Sub End
	 * Module Public Class GenericWithConstraint(Of T As Stream) End Class represents a
	 * value type, and the current instance represents Nullable<c> (Nullable(Of c) in
	 * Visual Basic). if none of these conditions are true, or if  is .
	 */
	function IsAssignableFrom(c:cs.system.Type):Bool;
	/**
	 * Returns a value that indicates whether the specified value exists in the current
	 * enumeration type.
	 * @param value The value to be tested.
	 * @return if the specified value is a member of the current enumeration type;
	 * otherwise, .
	 */
	function IsEnumDefined(value:Dynamic):Bool;
	/**
	 * Determines whether two COM types have the same identity and are eligible for
	 * type equivalence.
	 * @param other The COM type that is tested for equivalence with the current type.
	 * @return if the COM types are equivalent; otherwise, . This method also returns 
	 * if one type is in an assembly that is loaded for execution, and the other is in
	 * an assembly that is loaded into the reflection-only context.
	 */
	function IsEquivalentTo(other:cs.system.Type):Bool;
	/**
	 * Determines whether the specified object is an instance of the current .
	 * @param o The object to compare with the current type.
	 * @return if the current  is in the inheritance hierarchy of the object
	 * represented by , or if the current  is an interface that  implements.  if
	 * neither of these conditions is the case, if  is , or if the current  is an open
	 * generic type (that is,  returns ).
	 */
	function IsInstanceOfType(o:Dynamic):Bool;
	/**
	 * Determines whether the current  derives from the specified .
	 * @param c The type to compare with the current type.
	 * @return if the current  derives from ; otherwise, . This method also returns  if
	 * and the current  are equal.
	 */
	function IsSubclassOf(c:cs.system.Type):Bool;
	@:overload(function():cs.system.Type {})
	/**
	 * Returns a  object representing a one-dimensional array of the current type, with
	 * a lower bound of zero.
	 * @return A  object representing a one-dimensional array of the current type, with
	 * a lower bound of zero.
	 */
	function MakeArrayType(rank:Int):cs.system.Type;
	/**
	 * Returns a  object that represents the current type when passed as a  parameter (
	 * parameter in Visual Basic).
	 * @return A  object that represents the current type when passed as a  parameter (
	 * parameter in Visual Basic).
	 */
	function MakeByRefType():cs.system.Type;
	/**
	 * Substitutes the elements of an array of types for the type parameters of the
	 * current generic type definition and returns a  object representing the resulting
	 * constructed type.
	 * @param typeArguments An array of types to be substituted for the type parameters
	 * of the current generic type.
	 * @return A  representing the constructed type formed by substituting the elements
	 * of  for the type parameters of the current generic type.
	 */
	function MakeGenericType(typeArguments:cs.NativeArray<cs.system.Type>):cs.system.Type;
	/**
	 * Returns a  object that represents a pointer to the current type.
	 * @return A  object that represents a pointer to the current type.
	 */
	function MakePointerType():cs.system.Type;
	/**
	 * Returns a  representing the name of the current .
	 * @return A  representing the name of the current .
	 */
	function ToString():String;
}
