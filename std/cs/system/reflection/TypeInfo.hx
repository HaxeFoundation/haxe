package cs.system.reflection;

/** Represents type declarations for class types, interface types, array types, value types, enumeration types, type parameters, generic type definitions, and open or closed constructed generic types. */
@:native("System.Reflection.TypeInfo")
extern class TypeInfo extends cs.system.Type {
	/**
	 * Gets a collection of the constructors declared by the current type.
	 * @return A collection of the constructors declared by the current type.
	 */
	var DeclaredConstructors(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.ConstructorInfo>;
	/**
	 * Gets a collection of the events defined by the current type.
	 * @return A collection of the events defined by the current type.
	 */
	var DeclaredEvents(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.EventInfo>;
	/**
	 * Gets a collection of the fields defined by the current type.
	 * @return A collection of the fields defined by the current type.
	 */
	var DeclaredFields(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.FieldInfo>;
	/**
	 * Gets a collection of the members defined by the current type.
	 * @return A collection of the members defined by the current type.
	 */
	var DeclaredMembers(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.MemberInfo>;
	/**
	 * Gets a collection of the methods defined by the current type.
	 * @return A collection of the methods defined by the current type.
	 */
	var DeclaredMethods(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.MethodInfo>;
	/**
	 * Gets a collection of the nested types defined by the current type.
	 * @return A collection of nested types defined by the current type.
	 */
	var DeclaredNestedTypes(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.TypeInfo>;
	/**
	 * Gets a collection of the properties defined by the current type.
	 * @return A collection of the properties defined by the current type.
	 */
	var DeclaredProperties(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.PropertyInfo>;
	/**
	 * Gets an array of the generic type parameters of the current instance.
	 * @return An array that contains the current instance's generic type parameters,
	 * or an array of  zero if the current instance has no generic type parameters.
	 */
	var GenericTypeParameters(default, never):cs.NativeArray<cs.system.Type>;
	/**
	 * Gets a collection of the interfaces implemented by the current type.
	 * @return A collection of the interfaces implemented by the current type.
	 */
	var ImplementedInterfaces(default, never):cs.system.collections.generic.IEnumerable<cs.system.Type>;
	/**
	 * Returns the current type as a  object.
	 * @return The current type.
	 */
	function AsType():cs.system.Type;
	/**
	 * Returns an object that represents the specified public event declared by the
	 * current type.
	 * @param name The name of the event.
	 * @return An object that represents the specified event, if found; otherwise, .
	 */
	function GetDeclaredEvent(name:String):cs.system.reflection.EventInfo;
	/**
	 * Returns an object that represents the specified public field declared by the
	 * current type.
	 * @param name The name of the field.
	 * @return An object that represents the specified field, if found; otherwise, .
	 */
	function GetDeclaredField(name:String):cs.system.reflection.FieldInfo;
	/**
	 * Returns an object that represents the specified public method declared by the
	 * current type.
	 * @param name The name of the method.
	 * @return An object that represents the specified method, if found; otherwise, .
	 */
	function GetDeclaredMethod(name:String):cs.system.reflection.MethodInfo;
	/**
	 * Returns a collection that contains all public methods declared on the current
	 * type that match the specified name.
	 * @param name The method name to search for.
	 * @return A collection that contains methods that match .
	 */
	function GetDeclaredMethods(name:String):cs.system.collections.generic.IEnumerable<cs.system.reflection.MethodInfo>;
	/**
	 * Returns an object that represents the specified public nested type declared by
	 * the current type.
	 * @param name The name of the nested type.
	 * @return An object that represents the specified nested type, if found;
	 * otherwise, .
	 */
	function GetDeclaredNestedType(name:String):cs.system.reflection.TypeInfo;
	/**
	 * Returns an object that represents the specified public property declared by the
	 * current type.
	 * @param name The name of the property.
	 * @return An object that represents the specified property, if found; otherwise, .
	 */
	function GetDeclaredProperty(name:String):cs.system.reflection.PropertyInfo;
	/**
	 * Returns a value that indicates whether the specified type can be assigned to the
	 * current type.
	 * @param typeInfo The type to check.
	 * @return if the specified type can be assigned to this type; otherwise, .
	 */
	function IsAssignableFrom(typeInfo:cs.system.reflection.TypeInfo):Bool;
}
