package cs.system.reflection;

/** Wraps a  object and delegates methods to that . */
@:native("System.Reflection.TypeDelegator")
extern class TypeDelegator extends cs.system.reflection.TypeInfo {
	function new(delegatingType:cs.system.Type):Void;
	/**
	 * Returns an array of  objects representing constructors defined for the type
	 * wrapped by the current .
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return An array of type  containing the specified constructors defined for this
	 * class. If no constructors are defined, an empty array is returned. Depending on
	 * the value of a specified parameter, only public constructors or both public and
	 * non-public constructors will be returned.
	 */
	function GetConstructors(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.ConstructorInfo>;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Returns all the custom attributes defined for this type, specifying whether to
	 * search the type's inheritance chain.
	 * @param inherit Specifies whether to search this type's inheritance chain to find
	 * the attributes.
	 * @return An array of objects containing all the custom attributes defined for
	 * this type.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Returns the  of the object encompassed or referred to by the current array,
	 * pointer or ByRef.
	 * @return The  of the object encompassed or referred to by the current array,
	 * pointer or , or  if the current  is not an array, a pointer or a .
	 */
	function GetElementType():cs.system.Type;
	/**
	 * Returns the specified event.
	 * @param name The name of the event to get.
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return An  object representing the event declared or inherited by this type
	 * with the specified name. This method returns  if no such event is found.
	 */
	function GetEvent(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.EventInfo;
	@:overload(function():cs.NativeArray<cs.system.reflection.EventInfo> {})
	/**
	 * Returns an array of  objects representing all the public events declared or
	 * inherited by the current .
	 * @return An array that contains all the events declared or inherited by the
	 * current type. If there are no events, an empty array is returned.
	 */
	function GetEvents(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.EventInfo>;
	/**
	 * Returns a  object representing the field with the specified name.
	 * @param name The name of the field to find.
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return A  object representing the field declared or inherited by this  with the
	 * specified name. Returns  if no such field is found.
	 */
	function GetField(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.reflection.FieldInfo;
	/**
	 * Returns an array of  objects representing the data fields defined for the type
	 * wrapped by the current .
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return An array of type  containing the fields declared or inherited by the
	 * current . An empty array is returned if there are no matched fields.
	 */
	function GetFields(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.FieldInfo>;
	/**
	 * Returns the specified interface implemented by the type wrapped by the current .
	 * @param name The fully qualified name of the interface implemented by the current
	 * class.
	 * @param ignoreCase if the case is to be ignored; otherwise, .
	 * @return A  object representing the interface implemented (directly or
	 * indirectly) by the current class with the fully qualified name matching the
	 * specified name. If no interface that matches name is found, null is returned.
	 */
	function GetInterface(name:String, ignoreCase:Bool):cs.system.Type;
	/**
	 * Returns an interface mapping for the specified interface type.
	 * @param interfaceType The  of the interface to retrieve a mapping of.
	 * @return An  object representing the interface mapping for .
	 */
	function GetInterfaceMap(interfaceType:cs.system.Type):cs.system.reflection.InterfaceMapping;
	/**
	 * Returns all the interfaces implemented on the current class and its base
	 * classes.
	 * @return An array of type  containing all the interfaces implemented on the
	 * current class and its base classes. If none are defined, an empty array is
	 * returned.
	 */
	function GetInterfaces():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns members (properties, methods, constructors, fields, events, and nested
	 * types) specified by the given , , and .
	 * @param name The name of the member to get.
	 * @param type A bitmask that affects the way in which the search is conducted. The
	 * value is a combination of zero or more bit flags from .
	 * @param bindingAttr The type of members to get.
	 * @return An array of type  containing all the members of the current class and
	 * its base class meeting the specified criteria.
	 */
	function GetMember(name:String, type:cs.system.reflection.MemberTypes, bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Returns members specified by .
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return An array of type  containing all the members of the current class and
	 * its base classes that meet the  filter.
	 */
	function GetMembers(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MemberInfo>;
	/**
	 * Returns an array of  objects representing specified methods of the type wrapped
	 * by the current .
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return An array of  objects representing the methods defined on this .
	 */
	function GetMethods(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.MethodInfo>;
	/**
	 * Returns a nested type specified by  and in  that are declared or inherited by
	 * the type represented by the current .
	 * @param name The nested type's name.
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return A  object representing the nested type.
	 */
	function GetNestedType(name:String, bindingAttr:cs.system.reflection.BindingFlags):cs.system.Type;
	/**
	 * Returns the nested types specified in  that are declared or inherited by the
	 * type wrapped by the current .
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return An array of type  containing the nested types.
	 */
	function GetNestedTypes(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.Type>;
	/**
	 * Returns an array of  objects representing properties of the type wrapped by the
	 * current .
	 * @param bindingAttr A bitmask that affects the way in which the search is
	 * conducted. The value is a combination of zero or more bit flags from .
	 * @return An array of  objects representing properties defined on this .
	 */
	function GetProperties(bindingAttr:cs.system.reflection.BindingFlags):cs.NativeArray<cs.system.reflection.PropertyInfo>;
	/**
	 * Invokes the specified member. The method that is to be invoked must be
	 * accessible and provide the most specific match with the specified argument list,
	 * under the constraints of the specified binder and invocation attributes.
	 * @param name The name of the member to invoke. This may be a constructor, method,
	 * property, or field. If an empty string ("") is passed, the default member is
	 * invoked.
	 * @param invokeAttr The invocation attribute. This must be one of the following  :
	 * , , , , , , or . A suitable invocation attribute must be specified. If a static
	 * member is to be invoked, the  flag must be set.
	 * @param binder An object that enables the binding, coercion of argument types,
	 * invocation of members, and retrieval of  objects via reflection. If  is , the
	 * default binder is used. See .
	 * @param target The object on which to invoke the specified member.
	 * @param args An array of type  that contains the number, order, and type of the
	 * parameters of the member to be invoked. If  contains an uninitialized , it is
	 * treated as empty, which, with the default binder, can be widened to 0, 0.0 or a
	 * string.
	 * @param modifiers An array of type  that is the same length as , with elements
	 * that represent the attributes associated with the arguments of the member to be
	 * invoked. A parameter has attributes associated with it in the member's
	 * signature. For ByRef, use , and for none, use . The default binder does exact
	 * matching on these. Attributes such as  and  are not used in binding, and can be
	 * viewed using .
	 * @param culture An instance of  used to govern the coercion of types. This is
	 * necessary, for example, to convert a string that represents 1000 to a  value,
	 * since 1000 is represented differently by different cultures. If  is , the  for
	 * the current thread's  is used.
	 * @param namedParameters An array of type  containing parameter names that match
	 * up, starting at element zero, with the  array. There must be no holes in the
	 * array. If .  is greater than . , the remaining parameters are filled in order.
	 * @return An  representing the return value of the invoked member.
	 */
	function InvokeMember(name:String, invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, target:Dynamic, args:cs.NativeArray<Dynamic>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>, culture:cs.system.globalization.CultureInfo, namedParameters:cs.NativeArray<String>):Dynamic;
	/**
	 * Returns a value that indicates whether the specified type can be assigned to
	 * this type.
	 * @param typeInfo The type to check.
	 * @return if the specified type can be assigned to this type; otherwise, .
	 */
	function IsAssignableFrom(typeInfo:cs.system.reflection.TypeInfo):Bool;
	/**
	 * Indicates whether a custom attribute identified by  is defined.
	 * @param attributeType Specifies whether to search this type's inheritance chain
	 * to find the attributes.
	 * @param inherit An array of custom attributes identified by type.
	 * @return if a custom attribute identified by  is defined; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
}
